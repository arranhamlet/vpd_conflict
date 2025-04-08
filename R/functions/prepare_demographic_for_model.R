#' Prepare Demographic Inputs for Transmission Model
#'
#' This function processes demographic data (migration, fertility, mortality, and population)
#' into structured dfs and data frames suitable for input into an age-structured transmission model.
#'
#' @param migration Data frame with net migration rates by year.
#' @param fertility Data frame with fertility rates by age and year.
#' @param mortality Data frame with mortality counts by age and year.
#' @param population_all Data frame with total population counts by age and year.
#' @param population_female Data frame with female population counts by age and year.
#' @param year_start Start year for analysis (default: first year in migration).
#' @param year_end End year for analysis (default: last year in migration).
#' @param population_modifier Scalar to adjust population values.
#' @param fertility_modifier Scalar to adjust fertility values.
#' @param death_modifier Scalar to adjust mortality values (not currently used).
#' @param migration_modifier Scalar to adjust migration rates.
#' @param n_vacc Number of vaccination strata (default = 1).
#'
#' @return A named list containing prepared demographic components.
#' @export
prepare_demographic_for_model <- function(
    migration, fertility, mortality, population_all, population_female,
    year_start = "", year_end = "", population_modifier = 1,
    fertility_modifier = 1, death_modifier = 1, migration_modifier = 1,
    n_vacc = 1
) {
  # ---------------------- Subset to desired years ----------------------
  years <- if (year_start == "" & year_end == "") {
    min(migration$year, na.rm = TRUE):max(migration$year, na.rm = TRUE)
  } else if (year_start != "" & year_end == "") {
    as.numeric(year_start):max(migration$year, na.rm = TRUE)
  } else if (year_start == "" & year_end != "") {
    min(migration$year, na.rm = TRUE):as.numeric(year_end)
  } else {
    as.numeric(year_start):as.numeric(year_end)
  }
  
  time_run_for <- length(years)
  time_all <- seq(from = 0, length.out = time_run_for)
  
  # ---------------------- Population Formatting ----------------------
  total_population_data <- population_all %>%
    dplyr::filter(year %in% years) %>%
    dplyr::select(x0:x100) * population_modifier
  
  population_all_vector <- rowSums(as.matrix(total_population_data))
  
  # ---------------------- Mortality Processing ----------------------
  mortality_change <- mortality %>%
    dplyr::filter(year %in% years) %>%
    dplyr::select(x0:x100)
  
  mortality_correct_format <- mortality_change / total_population_data
  is.na(mortality_correct_format) <- sapply(mortality_correct_format, is.infinite)
  mortality_correct_format[is.na(mortality_correct_format)] <- 1
  
  mortality_vector <- mortality_correct_format %>%
    as.matrix() %>%
    t() %>%
    as.vector() %>%
    pmin(1)
  
  # ---------------------- Fertility Processing ----------------------
  pop_fem <- population_female %>%
    dplyr::filter(year %in% years) %>%
    dplyr::select(x15:x49) %>%
    as.matrix() * population_modifier
  
  fert <- fertility %>%
    dplyr::filter(year %in% years) %>%
    dplyr::select(x15:x49) %>%
    as.matrix() * fertility_modifier
  
  fertility_by_year <- rowSums((fert / 1000) * pop_fem) / rowSums(pop_fem)
  
  # ---------------------- Migration Calculation ----------------------
  net_migration <- migration %>%
    dplyr::filter(year %in% years) %>%
    dplyr::select(net_migration_rate_per_1_000_population) %>%
    dplyr::rename(migration_per_1000 = 1) %>%
    as.matrix() %>%
    as.vector() * migration_modifier
  
  migration_upd <- do.call(rbind, lapply(1:nrow(total_population_data), function(x) {
    data.frame(
      dim1 = x, dim2 = 1:101, dim3 = 1, dim4 = 1,
      value = round(as.numeric(total_population_data[x, ] * net_migration[x] / 1000) * 1000, 0)
    )
  }))
  
  # ---------------------- Initial Population ----------------------
  N0_df <- data.frame(
    dim1 = 1:101, dim2 = 1, dim3 = 1,
    value = round(as.numeric(total_population_data[1, ]) * 1000, 0)
  )
  
  # ---------------------- Population Data Data.frame ----------------------
  total_population_df <- do.call(rbind, lapply(1:nrow(total_population_data), function(x) {
    data.frame(
      dim1 = x, dim2 = 1:101, dim3 = 1, dim4 = 1,
      value = round(as.numeric(total_population_data[x, ]) * 1000, 0)
    )
  }))
  
  # ---------------------- Mortality Data.frame ----------------------
  mortality_df <- data.table::rbindlist(lapply(1:nrow(mortality_correct_format), function(x) {
    data.frame(
      dim1 = x, dim2 = 1:101, dim3 = 1,
      value = as.numeric(mortality_correct_format[x, ])
    )
  })) %>% as.data.frame()
  
  # ---------------------- Migration Distribution ----------------------
  migration_distribution_values <- expand.grid(
    dim1 = 1:length(time_all), dim2 = 1,
    dim3 = 1:ncol(total_population_data), dim4 = 1,
    dim5 = 1, value = 1
  )
  
  # ---------------------- Output ----------------------
  list(
    N0 = N0_df,
    crude_birth = pmin(fertility_by_year, 1),
    crude_death = pmin(mortality_vector, 1),
    tt_migration = time_all,
    migration_in_number = migration_upd,
    migration_distribution_values = migration_distribution_values,
    population_data = total_population_data,
    years = years
  )
}
