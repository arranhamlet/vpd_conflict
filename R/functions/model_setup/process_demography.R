#' Prepare Demographic Inputs for Transmission Model
#'
#' This function processes demographic data (migration, fertility, mortality, and population)
#' into structured data frames suitable for input into an age-, vaccination-, and risk-structured
#' infectious disease transmission model.
#'
#' @param migration Data frame of net migration rates by year and country.
#' @param fertility Data frame of fertility rates by single year of age and year.
#' @param mortality Data frame of mortality counts by single year of age and year.
#' @param population_all Data frame of total population counts by age and year.
#' @param population_female Data frame of female population counts by age and year.
#' @param iso3 A 3-letter ISO country code to subset the data.
#' @param year_start First year to include (default: first year in migration data).
#' @param year_end Final year to include (default: last year in migration data).
#' @param population_modifier Scalar to multiply population values.
#' @param fertility_modifier Scalar to multiply fertility rates.
#' @param death_modifier Scalar to multiply death rates (not used).
#' @param migration_modifier Scalar to multiply migration rates.
#' @param n_age Number of age bins to collapse to.
#' @param n_vacc Number of vaccination strata.
#' @param n_risk Number of risk strata.
#'
#' @return A named list with prepared demographic inputs for a transmission model.
#' @export

process_demography <- function(
    migration, fertility, mortality, population_all, population_female,
    iso,
    year_start = "", 
    year_end = "",
    population_modifier = 1, 
    fertility_modifier = 1, 
    death_modifier = 1,
    migration_modifier = 1,
    n_age = 1, 
    n_vacc = 1, 
    n_risk = 1
) {

  filter_country <- function(dt) dt[iso3 == iso]

  # Convert to data.table - for speed
  setDT(migration); setDT(fertility); setDT(mortality); setDT(population_all); setDT(population_female)
  
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
  pop_all_raw <- as.matrix(population_all[year %in% years, paste0("x", 0:100), with = FALSE]) * population_modifier
  pop_all <- collapse_age_bins(pop_all_raw, n_age)
  
  # Mortality
  mort_mat_raw <- as.matrix(mortality[year %in% years, paste0("x", 0:100), with = FALSE])
  mort_mat <- collapse_age_bins(mort_mat_raw, n_age)
  mortality_rate <- mort_mat / pop_all
  mortality_rate[!is.finite(mortality_rate)] <- 1
  mortality_df <- reshape2::melt(t(mortality_rate)) %>%
    setnames(c("n_age", "time", "value"))
  
  # Fertility
  fert_mat <- as.matrix(fertility[year %in% years, paste0("x", 15:49), with = FALSE]) * fertility_modifier
  pop_fem <- as.matrix(population_female[year %in% years, paste0("x", 15:49), with = FALSE]) * population_modifier
  denom <- rowSums(pop_fem)
  denom[denom == 0] <- NA
  fertility_by_year <- data.frame(time = time_all, value = pmin(rowSums((fert_mat / 1000) * pop_fem) / denom, 1))
  
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
  N0_df <- data.frame(
    dim1 = seq_len(n_age),
    dim2 = 1,
    dim3 = 1,
    value = init_chunk
  )
  
  # Mortality df
  mortality_df <- rbindlist(lapply(seq_len(nrow(mortality_rate)), function(i) {
    chunk <- split_and_sum(mortality_rate[i, ], n_age)
    data.frame(
      dim1 = i,
      dim2 = seq_len(n_age),
      dim3 = 1,
      value = chunk
    )
  }))
  
  # Total population df (optional)
  total_population_df <- rbindlist(lapply(seq_len(nrow(pop_all)), function(i) {
    chunk <- split_and_sum(round(pop_all[i, ] * 1000), n_age)
    data.frame(
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
    crude_birth = fertility_by_year,
    crude_death = mortality_df,
    tt_migration = time_all,
    migration_in_number = migration_in_number,
    migration_distribution_values = migration_distribution_values,
    population_data = pop_all,
    input_data =   data.frame(
      iso = iso,
      year_start = min(years), 
      year_end = max(years),
      population_modifier = population_modifier, 
      fertility_modifier = fertility_modifier, 
      death_modifier = death_modifier,
      migration_modifier = n_age,
      n_age = n_age, 
      n_vacc = n_vacc, 
      n_risk = n_risk
    )

  )
  
}
