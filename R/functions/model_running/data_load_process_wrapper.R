

data_load_process_wrapper <- function(
    iso,
    disease,
    vaccine,
    R0,
    timestep = "day",
    year_start = "",
    year_end = "",
    WHO_seed_switch = F
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
  vaccination_translated <- import("data/processed/vaccination/Vaccine_Abbreviations_and_Diseases.csv")
  
  routine_vaccination_data <- routine_vaccination_data %>%
    left_join(vaccination_translated, by = c("ANTIGEN" = "Abbreviation"))
  
  disease_parameters <- import("data/processed/model_parameters/disease_parameters_table.xlsx") %>%
    rename(disease_n = disease) %>%
    subset(disease_n == disease)
  
  vaccine_parameters <- import("data/processed/vaccination/vaccine_protection.xlsx") %>%
    rename(disease_n = disease) %>%
    subset(disease_n == disease)
  
  #Calculate number of vaccines
  number_of_vaccines <- routine_vaccination_data %>%
    subset(CODE == iso & (grepl(vaccine, ANTIGEN_DESCRIPTION, ignore.case = T) | grepl(disease, Disease, ignore.case = T))) %>%
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
  
  #Add in vaccine provided protection
  n_vacc <- model_data_preprocessed$processed_demographic_data$input_data$n_vacc
  
  vacc_order <- seq(2, n_vacc, by = 2)
  age_vaccination_beta_modifier <- Reduce(rbind, sapply(vacc_order, function(j){
    
    dose_details <- vaccine_parameters %>%
      mutate(order = abs(j - vaccine_parameters$dose)) %>%
      subset(order == min(order))
    
    rbind(
      expand.grid(
        dim1 = 1:101,
        dim2 = j,
        dim3 = 1,
        value = dose_details %>% subset(parameter == "short_term_protection") %>% pull(value)
      ),
      expand.grid(
        dim1 = 1:101,
        dim2 = j + 1,
        dim3 = 1,
        value = dose_details %>% subset(parameter == "long_term_protection") %>% pull(value)
      )
    )
    
  }, simplify = FALSE))
  
  
  time_adjust <- if(timestep == "day"){
    1
  } else if(timestep == "week"){
    7
  } else if(timestep == "month"){
    30 
  } else if(timestep == "quarter"){
    91.25
  } else if(timestep == "year"){
    365
  }
  
  #Set up model
  time_changes_mig <- model_data_preprocessed$processed_demographic_data$tt_migration * 365/time_adjust
  time_changes_mig <- floor(c(time_changes_mig, max(time_changes_mig) + 1))
  
  time_changes_vac <- case_vaccination_ready$tt_vaccination * 365/time_adjust
  time_changes_vac <- floor(c(time_changes_vac, max(time_changes_vac) + 1))
  
  time_changes_seeded <- case_vaccination_ready$tt_seeded * 365/time_adjust
  R0_switch_time <- time_changes_seeded[2]
  
  if(WHO_seed_switch == T){
    
    time_changes_seeded <- c(sapply(time_changes_seeded, function(e){
      c(e, e + 1)
    }))
    
    time <- sort(unique(case_vaccination_ready$seeded$dim4))
    
    seed_with_data <- case_vaccination_ready$seeded %>%
      mutate(dim4 = dim4 * 2 - 1)
    
    seed_without_data <- case_vaccination_ready$seeded %>%
      mutate(dim4 = dim4 * 2,
             value = 0)
    
    seed <- rbind(seed_with_data,
                  seed_without_data)
    
  } else{
    time_changes_seeded <- floor(c(time_changes_seeded, max(time_changes_seeded) + 1))
  }
  
  
  params <- param_packager(
    
    # Demographic parameters
    n_age = model_data_preprocessed$processed_demographic_data$input_data$n_age,
    n_vacc = model_data_preprocessed$processed_demographic_data$input_data$n_vacc,
    n_risk = model_data_preprocessed$processed_demographic_data$input_data$n_risk,
    
    # Vaccine parameters
    short_term_waning = 1/((vaccine_parameters %>% subset(parameter == "short_term_waning") %>% pull(value) %>% as.numeric() %>% max() * 365)/time_adjust),
    long_term_waning = 1/((vaccine_parameters %>% subset(parameter == "long_term_waning") %>% pull(value) %>% as.numeric() %>% max() * 365)/time_adjust),
    age_vaccination_beta_modifier = age_vaccination_beta_modifier,
    
    # Disease parameters 
    R0 = if(WHO_seed_switch == T) c(R0, 0) else R0,
    tt_R0 = if(WHO_seed_switch == T) c(0, R0_switch_time) else 0,
    user_specified_foi = 0,
    
    #Disease parameters
    cfr_normal = 0,
    cfr_severe = 0,
    incubation_rate = 1/subset(disease_parameters, parameter == "incubation period") %>% pull(value) %>% as.numeric() * time_adjust,
    recovery_rate = 1/subset(disease_parameters, parameter == "infectious period") %>% pull(value) %>% as.numeric() * time_adjust,
    severe_recovery_rate = 1/subset(disease_parameters, parameter == "infectious period") %>% pull(value) %>% as.numeric() * time_adjust,
    
    #Setting up vaccination
    vaccination_coverage = case_vaccination_ready$vaccination_coverage,
    
    #Demographic parameters
    contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
    N0 = model_data_preprocessed$processed_demographic_data$N0,
    
    I0 = if(WHO_seed_switch == T) data.frame(dim1 = 18, dim2 = 1, dim3 = 1, value = 100) else 0,
    
    #Time of changes
    tt_birth_changes = time_changes_mig,
    tt_death_changes = time_changes_mig,
    tt_migration = time_changes_mig,
    tt_vaccination_coverage = time_changes_vac, 
    
    #List of when birth_death_changes
    crude_birth = model_data_preprocessed$processed_demographic_data$crude_birth %>%
      mutate(value = value/(365/time_adjust)),
    crude_death = model_data_preprocessed$processed_demographic_data$crude_death %>%
      mutate(value = value/(365/time_adjust)),
    aging_rate = time_adjust/365,
    migration_in_number = model_data_preprocessed$processed_demographic_data$migration_in_number %>%
      mutate(value = value/(365/time_adjust)),
    migration_distribution_values = model_data_preprocessed$processed_demographic_data$migration_distribution_values,
    
    tt_seeded = if(WHO_seed_switch == T) time_changes_seeded else c(0, max(time_changes_seeded)),
    seeded = if(WHO_seed_switch == T) seed else expand.grid(dim1 = 18, dim2 = 1, dim3 = 1, dim4 = 1, dim5 = 1:2, value = 10),
    
    #Birth ages
    repro_low = 15,
    repro_high = 49,
    age_maternal_protection_ends = 1,
    protection_weight_vacc = 1,
    protection_weight_rec = 1,
    migration_represent_current_pop = 1
    
  )
  
  #Export these
  list(
    params = params,
    time = c((model_data_preprocessed$processed_demographic_data$input_data$year_end - model_data_preprocessed$processed_demographic_data$input_data$year_start) + 1) * 365/time_adjust,
    input_data = model_data_preprocessed$processed_demographic_data$input_data %>%
      mutate(time_adjust = time_adjust)
  )
  
}
