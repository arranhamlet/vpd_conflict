
model_input_formatter_wrapper <- function(
  #Datasets to be loaded and input
  migration,
  fertility,
  mortality,
  population_all,
  population_female,
  #Iso3 country code
  iso,
  #WHO disease data
  disease_data,
  #Disease of interest
  disease,
  #Pre-calculated vaccination data
  vaccination_data,
  #Vaccine of interest
  vaccine,
  
  year_start = "",
  year_end = "",
  n_age = 1,
  n_vacc = 1,
  n_risk = 1
  
  ){
  
  #Calculate demographics
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
    n_vacc = n_vacc,
    n_risk = n_risk
  )
  
  #Disease data
  processed_disease <- process_prior_cases(
    disease_data = disease_data,
    disease = disease,
    iso = demographic_data_calculated$input_data$iso,
    year_start = demographic_data_calculated$input_data$year_start,
    year_end = demographic_data_calculated$input_data$year_end
  )
  
  #Vaccination data
  processed_vaccination <- process_vaccination(
    vaccination_data = vaccination_data,
    vaccine = vaccine,
    iso = demographic_data_calculated$input_data$iso,
    year_start = demographic_data_calculated$input_data$year_start,
    year_end = demographic_data_calculated$input_data$year_end
  )
  
  #Plot some illustrations of the data
  case_vaccination_plots <- plot_case_vaccination(
    case_data = processed_disease,
    vaccination_data = processed_vaccination
  )
  
  demographic_plots <- plot_demographic_data(
    demographic_data_calculated
  )
  
  plotting <- wrap_elements(case_vaccination_plots)/wrap_elements(demographic_plots) + plot_layout(heights = c(1.5, 2))
  
  plotting
  
  list(
    processed_demographic_data = demographic_data_calculated,
    processed_case_data = processed_disease,
    processed_vaccination_data = processed_vaccination,
    demographic_plots = plotting
  )
  
}