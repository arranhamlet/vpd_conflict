data_load_process_wrapper <- function(
  iso,
  disease,
  vaccine,
  R0,
  timestep = "day",
  year_start = "",
  year_end = ""
  ){
  
  #Load files needed
  migration <- import(here("data", "processed", "WPP", "migration.csv"))
  fertility <- import(here("data", "processed", "WPP", "fertility.csv"))
  mortality <- import(here("data", "processed", "WPP", "deaths.csv"))
  population_all <- import(here("data", "processed", "WPP", "age_both.csv"))
  population_female <- import(here("data", "processed", "WPP", "age_female.csv"))
  contact_matricies <- import(here("data", "raw", "contact_matricies", "contact_all.rdata"))
  routine_vaccination_data <- import("data/raw/WHO/coverage-data_updated.xlsx")
  sia_vaccination <- import("data/processed/vaccination/sia_vimc.rds")
  full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")
  vaccination_schedule <- import("data/processed/WHO/vaccine-schedule-data.xlsx")
  measles_parameters <- import(here("data", "processed", "model_parameters", "Measles_SEIR_Parameters.csv"))
  
  #Calculate number of vaccines
  number_of_vaccines <- routine_vaccination_data %>%
    subset(CODE == iso & grepl(vaccine, ANTIGEN_DESCRIPTION, ignore.case = T)) %>%
    pull(ANTIGEN) %>%
    unique()

  #Run processing
  model_data_preprocessed <- model_input_formatter_wrapper(
    iso = iso,    
    disease = disease,
    vaccine = vaccine,
    n_age = 101,
    number_of_vaccines = length(number_of_vaccines),
    #Datasets
    migration,
    fertility,
    mortality,
    population_all,
    population_female,
    contact_matricies = contact_matricies,
    disease_data = full_disease_df,
    vaccination_data_routine = routine_vaccination_data,
    vaccination_data_sia = sia_vaccination,
    year_start = year_start,
    year_end = year_end
  )
  
  #Take pre-processed case and vaccination data and get it ready for params
  case_vaccination_ready <- case_vaccine_to_param(
    demog_data = model_data_preprocessed$processed_demographic_data,
    processed_vaccination = model_data_preprocessed$processed_vaccination_data,
    processed_vaccination_sia = model_data_preprocessed$processed_vaccination_sia,
    processed_case = model_data_preprocessed$processed_case_data,
    vaccination_schedule = vaccination_schedule
  )
  
  age_vaccination_beta_modifier <- rbind(
    expand.grid(
      dim1 = 1:101,
      dim2 = 2:3,
      dim3 = 1,
      value = subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("1 dose", description)) %>% pull(value)/100
    ),
    expand.grid(
      dim1 = 1:101,
      dim2 = 4:5,
      dim3 = 1,
      value = subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("2 dose", description)) %>% pull(value)/100
    )
  )
  
  time_adjust <- if(timestep == "day"){
    1
  } else if(timestep == "week"){
    7
  } else if(timestep == "month"){
    30 
  } else if(timestep == "year"){
    365
  }
  
  initial_FOI <- infection_proportion_per_timestep(
    R0 = 18,
    contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
    D = measles_parameters %>% subset(parameter == "recovery_rate") %>% pull(value),
    population_vector = model_data_preprocessed$processed_demographic_data$N0[, 4],
    timestep_length = time_adjust
  )$infection_proportion_per_timestep
  
  
  final_epidemic_size_structured(
    contact_matrix = contact_matricies$GBR,#model_data_preprocessed$processed_demographic_data$contact_matrix,
    R0 = 2,
    D = measles_parameters %>% subset(parameter == "recovery_rate") %>% pull(value),
    tol = 1e-8, 
    max_iter = 1000
    )
    
  
  
  #Set up model
  time_changes_mig <- model_data_preprocessed$processed_demographic_data$tt_migration * 365/time_adjust
  time_changes_mig <- floor(c(time_changes_mig, max(time_changes_mig) + 1))
  
  time_changes_vac <- case_vaccination_ready$tt_vaccination * 365/time_adjust
  time_changes_vac <- floor(c(time_changes_vac, max(time_changes_vac) + 1))
  
  time_changes_seeded <- case_vaccination_ready$tt_seeded * 365/time_adjust
  time_changes_seeded <- floor(c(time_changes_seeded, max(time_changes_seeded) + 1))
  
  params <- param_packager(
    
    # Demographic parameters
    n_age = model_data_preprocessed$processed_demographic_data$input_data$n_age,
    n_vacc = model_data_preprocessed$processed_demographic_data$input_data$n_vacc,
    n_risk = model_data_preprocessed$processed_demographic_data$input_data$n_risk,
    
    # Vaccine parameters
    short_term_waning = 1/(14/time_adjust),
    long_term_waning = 0,
    age_vaccination_beta_modifier = age_vaccination_beta_modifier,
    
    # Disease parameters 
    R0 = R0,
    tt_R0 = 0,
    user_specified_foi = 1,
    initial_FOI = initial_FOI,
    
    #Disease parameters
    cfr_normal = 0,
    cfr_severe = 0,
    incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value) * time_adjust,
    recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * time_adjust,
    severe_recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * time_adjust,
    
    #Setting up vaccination
    vaccination_coverage = case_vaccination_ready$vaccination_coverage,
    
    #Demographic parameters
    contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
    N0 = model_data_preprocessed$processed_demographic_data$N0,
    
    #Time of changes
    tt_birth_changes = time_changes,
    tt_death_changes = time_changes,
    tt_migration = time_changes,
    tt_vaccination_coverage = time_changes_vac, 
    
    #List of when birth_death_changes
    crude_birth = model_data_preprocessed$processed_demographic_data$crude_birth %>%
      mutate(value = value/(365/time_adjust)),
    crude_death = model_data_preprocessed$processed_demographic_data$crude_death %>%
      mutate(value = value/(365/time_adjust)),
    simp_birth_death = 0,
    aging_rate = time_adjust/365,
    migration_in_number = model_data_preprocessed$processed_demographic_data$migration_in_number %>%
      mutate(value = value/(365/time_adjust)),
    migration_distribution_values = model_data_preprocessed$processed_demographic_data$migration_distribution_values,
    
    #Birth ages
    repro_low = 15,
    repro_high = 49,
    age_maternal_protection_ends = 1,
    protection_weight_vacc = 1,
    protection_weight_rec = 1,
    migration_represent_current_pop = 1
    
  )
  
  #Run model
  clean_df <- run_model(
    odin_model = model,
    params = params,
    time = c((model_data_preprocessed$processed_demographic_data$input_data$year_end - model_data_preprocessed$processed_demographic_data$input_data$year_start) + 1) * 365/time_adjust,
    no_runs = 1
  )
  
  
  
}