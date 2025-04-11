#' Prepare Demographic Inputs for Transmission Model
#'
#' This function processes demographic data (migration, fertility, mortality, and population)
#' into structured data frames suitable for input into an age-, vaccination-, and risk-structured
#' infectious disease transmission model. It calculates time-varying demographic quantities such as
#' net migration, fertility, and mortality, and outputs formatted data for Odin model use.
#'
#' @param migration A data frame of net migration rates by year and country.
#' @param fertility A data frame of fertility rates by single year of age and year.
#' @param mortality A data frame of mortality counts by single year of age and year.
#' @param population_all A data frame of total population counts by age and year.
#' @param population_female A data frame of female population counts by age and year (used for fertility).
#' @param iso3 A 3-letter ISO country code to subset the data to the relevant country.
#' @param year_start First year to include in model input. If blank, uses the first year in `migration`.
#' @param year_end Final year to include in model input. If blank, uses the last year in `migration`.
#' @param population_modifier Scalar multiplier applied to total and female population values.
#' @param fertility_modifier Scalar multiplier applied to fertility rates.
#' @param death_modifier Scalar multiplier for deaths (currently unused).
#' @param migration_modifier Scalar multiplier for net migration rates.
#' @param n_age Number of age groups to collapse the population into.
#' @param n_vacc Number of vaccination strata (default = 1).
#' @param n_risk Number of risk strata (default = 1).
#'
#' @return A named list containing:
#' \describe{
#'   \item{N0}{Data frame with initial population by age, vacc, and risk strata.}
#'   \item{crude_birth}{Vector of annual crude birth rates.}
#'   \item{crude_death}{Vector of annual crude death rates.}
#'   \item{tt_migration}{Vector of model time points corresponding to years.}
#'   \item{migration_in_number}{Data table with net migration in numbers, stratified by age, time.}
#'   \item{migration_distribution_values}{Data table with equal weights over migration strata.}
#'   \item{population_data}{Matrix of full population values by year and single-year age.}
#'   \item{years}{Vector of years used in the model.}
#'   \item{n_age}{Number of age strata.}
#'   \item{n_vacc}{Number of vaccination strata.}
#'   \item{n_risk}{Number of risk strata.}
#' }
#' @export
prepare_demographic_for_model <- function(
    migration, fertility, mortality, population_all, population_female,
    iso3,
    year_start = "", year_end = "",
    population_modifier = 1, fertility_modifier = 1, death_modifier = 1,
    migration_modifier = 1,
    n_age = 1, n_vacc = 1, n_risk = 1
) {
  library(data.table)
  
  # Fast group summing function
  split_and_sum <- function(vec, n_bins) {
    group_size <- ceiling(length(vec) / n_bins)
    groups <- (seq_along(vec) - 1) %/% group_size + 1
    tapply(vec, groups, sum)
  }
  
  filter_country <- function(dt) dt[iso3_alpha_code == iso3]
  get_years <- function(yrs, start, end) {
    start <- if (start == "") min(yrs, na.rm = TRUE) else as.numeric(start)
    end <- if (end == "") max(yrs, na.rm = TRUE) else as.numeric(end)
    start:end
  }
  
  # Convert to data.table
  setDT(migration); setDT(fertility); setDT(mortality)
  setDT(population_all); setDT(population_female)
  
  # Filter
  migration <- filter_country(migration)
  fertility <- filter_country(fertility)
  mortality <- filter_country(mortality)
  population_all <- filter_country(population_all)
  population_female <- filter_country(population_female)
  
  # Time window
  years <- get_years(migration$year, year_start, year_end)
  time_run_for <- length(years)
  time_all <- 0:(time_run_for - 1)
  
  # Base population
  pop_all <- as.matrix(population_all[year %in% years, paste0("x", 0:100), with = FALSE]) * population_modifier
  
  # Mortality
  mort_mat <- as.matrix(mortality[year %in% years, paste0("x", 0:100), with = FALSE])
  mortality_rate <- mort_mat / pop_all
  mortality_rate[!is.finite(mortality_rate)] <- 1
  mortality_vector <- pmin(as.vector(t(mortality_rate)), 1)
  
  # Fertility
  fert_mat <- as.matrix(fertility[year %in% years, paste0("x", 15:49), with = FALSE]) * fertility_modifier
  pop_fem <- as.matrix(population_female[year %in% years, paste0("x", 15:49), with = FALSE]) * population_modifier
  denom <- rowSums(pop_fem)
  denom[denom == 0] <- NA
  fertility_by_year <- rowSums((fert_mat / 1000) * pop_fem) / denom
  
  # Migration
  mig_rates <- migration[year %in% years, migration_rate_1000] * migration_modifier
  
  migration_in_number <- rbindlist(lapply(seq_len(nrow(pop_all)), function(i) {
    mig_vals <- round(pop_all[i, ] * mig_rates[i])
    chunk <- split_and_sum(mig_vals, n_age)
    data.table(
      dim1 = i,
      dim2 = seq_len(n_age),
      dim3 = 1,
      dim4 = 1,
      value = chunk
    )
  }))
  
  # Initial population
  init_vals <- round(pop_all[1, ] * 1000)
  init_chunk <- split_and_sum(init_vals, n_age)
  N0_df <- data.table(
    dim1 = seq_len(n_age),
    dim2 = 1,
    dim3 = 1,
    value = init_chunk
  )
  
  # Mortality df
  mortality_df <- rbindlist(lapply(seq_len(nrow(mortality_rate)), function(i) {
    chunk <- split_and_sum(mortality_rate[i, ], n_age)
    data.table(
      dim1 = i,
      dim2 = seq_len(n_age),
      dim3 = 1,
      value = chunk
    )
  }))
  
  # Total population df (optional)
  total_population_df <- rbindlist(lapply(seq_len(nrow(pop_all)), function(i) {
    chunk <- split_and_sum(round(pop_all[i, ] * 1000), n_age)
    data.table(
      dim1 = i,
      dim2 = seq_len(n_age),
      dim3 = 1,
      dim4 = 1,
      value = chunk
    )
  }))
  
  # Migration distribution
  migration_distribution_values <- CJ(
    dim1 = seq_along(time_all),
    dim2 = 1,
    dim3 = 1:n_age,
    dim4 = 1,
    dim5 = 1
  )[, value := 1]
  
  # Output
  list(
    N0 = N0_df,
    crude_birth = pmin(fertility_by_year, 1),
    crude_death = pmin(mortality_vector, 1),
    tt_migration = time_all,
    migration_in_number = migration_in_number,
    migration_distribution_values = migration_distribution_values,
    population_data = pop_all,
    years = years,
    n_age = n_age,
    n_vacc = 1,
    n_risk = 1
  )
}
