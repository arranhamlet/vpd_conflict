#' Format and Process Model Inputs for an Infectious Disease Transmission Model
#'
#' This wrapper function orchestrates the preparation of demographic, disease, and vaccination data
#' for use in an age-, vaccination-, and risk-structured infectious disease model.
#' It processes raw data sources, subsets them by country and time window, and generates illustrative plots.
#'
#' @param migration A data frame of net migration rates by year and country. Passed to `process_demography()`.
#' @param fertility A data frame of fertility rates by age and year. Passed to `process_demography()`.
#' @param mortality A data frame of mortality counts by age and year. Passed to `process_demography()`.
#' @param population_all A data frame of total population counts by age and year. Passed to `process_demography()`.
#' @param population_female A data frame of female population counts by age and year. Passed to `process_demography()`.
#' @param iso A 3-letter ISO country code for subsetting the data.
#' @param disease_data A data frame of WHO-reported case data across countries and years.
#' @param disease A string indicating the disease of interest (e.g., `"measles"`).
#' @param vaccination_data A data frame containing vaccine coverage and population data by country and year.
#' @param vaccine A string specifying the vaccine of interest (e.g., `"measles"`).
#' @param year_start First year to include (default: determined from migration data).
#' @param year_end Final year to include (default: determined from migration data).
#' @param n_age Number of age groups in the model.
#' @param n_vacc Number of vaccination groups in the model.
#' @param n_risk Number of risk groups in the model.
#'
#' @return A named list containing:
#' \describe{
#'   \item{processed_demographic_data}{A list from `process_demography()` with demographic inputs.}
#'   \item{processed_case_data}{A data frame of case data processed via `process_prior_cases()`.}
#'   \item{processed_vaccination_data}{A filtered and formatted vaccination data frame.}
#'   \item{demographic_plots}{A combined `patchwork` plot of case, vaccination, and demographic data.}
#' }
#'
#' @details
#' This function calls and links the following internally defined functions:
#' \code{process_demography()}, \code{process_prior_cases()}, \code{process_vaccination()},
#' \code{plot_case_vaccination()}, and \code{plot_demographic_data()}.
#' It also produces summary plots using `patchwork::wrap_elements()` and `plot_layout()`.
#'
#' @import patchwork
#' @importFrom patchwork wrap_elements
#'
#' @export
model_input_formatter_wrapper <- function(
  # Datasets to be loaded and input
  migration,
  fertility,
  mortality,
  population_all,
  population_female,
  iso,
  disease_data,
  disease,
  vaccination_data_routine,
  vaccination_data_sia,
  vaccine,
  contact_matricies,
  year_start = "",
  year_end = "",
  n_age = 1,
  number_of_vaccines = 1,
  n_risk = 1
) {
  
  # Step 1: Calculate demographics
  demographic_data_calculated <- process_demography(
    migration = migration, 
    fertility = fertility, 
    mortality = mortality, 
    population_all = population_all, 
    population_female = population_female,
    iso = iso,
    year_start = year_start,
    year_end = year_end,
    n_age = n_age,
    number_of_vaccines = number_of_vaccines,
    n_risk = n_risk,
    contact_matricies = contact_matricies
  )
  
  #Filter vaccination and case data to max year in demographic data
  vaccination_data_routine <- vaccination_data_routine %>% 
    filter(YEAR <= demographic_data_calculated$input_data$year_end)
  
  disease_data <- disease_data %>% 
    filter(year <= demographic_data_calculated$input_data$year_end)
  
  # Step 2: Process disease data
  processed_disease <- process_prior_cases(
    disease_data = disease_data,
    disease = disease,
    iso = demographic_data_calculated$input_data$iso,
    year_start = demographic_data_calculated$input_data$year_start,
    year_end = demographic_data_calculated$input_data$year_end
  )
  
  # Step 3: Process routine vaccination data
  processed_vaccination <- process_vaccination_routine(
    vaccination_data = vaccination_data_routine,
    vaccine = vaccine,
    iso = demographic_data_calculated$input_data$iso,
    year_start = demographic_data_calculated$input_data$year_start,
    year_end = demographic_data_calculated$input_data$year_end
  )
  
  # Step 4: Process supplemental immunization activities
  processed_vaccination_sia <- process_vaccination_sia(
    vaccination_data = vaccination_data_sia,
    vaccine = vaccine,
    iso = demographic_data_calculated$input_data$iso,
    year_start = demographic_data_calculated$input_data$year_start,
    year_end = demographic_data_calculated$input_data$year_end
  )

  # Step 5: Return formatted objects
  find_maximum_year <- max(c(demographic_data_calculated$year, processed_disease$year, processed_vaccination$year))
  
  list(
    processed_demographic_data = demographic_data_calculated,
    processed_case_data = processed_disease,
    processed_vaccination_data = processed_vaccination,
    processed_vaccination_sia = processed_vaccination_sia
  )
}
