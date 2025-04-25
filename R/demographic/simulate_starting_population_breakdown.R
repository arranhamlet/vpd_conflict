if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  rio,
  here,
  dust2,
  tidyverse,
  reshape2,
  collapse,
  janitor,
  patchwork,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Load files needed
migration <- import(here("data", "processed", "WPP", "migration.csv"))
fertility <- import(here("data", "processed", "WPP", "fertility.csv"))
mortality <- import(here("data", "processed", "WPP", "deaths.csv"))
population_all <- import(here("data", "processed", "WPP", "age_both.csv"))
population_female <- import(here("data", "processed", "WPP", "age_female.csv"))
contact_matricies <- import(here("data", "raw", "contact_matricies", "contact_all.rdata"))

routine_vaccination_data <- import("data/raw/WHO/coverage-data_updated.xlsx")
full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")
vaccination_schedule <- import("data/processed/WHO/WHO_vaccination_schedule.xlsx")
measles_parameters <- import(here("data", "processed", "model_parameters", "Measles_SEIR_Parameters.csv"))

#Run processing
model_data_preprocessed <- model_input_formatter_wrapper(
  iso = "PSE",    
  disease = "measles",
  vaccine = "measles",
  n_age = 101,
  number_of_vaccines = 2,
  #Datasets
  migration,
  fertility,
  mortality,
  population_all,
  population_female,
  contact_matricies = contact_matricies,
  disease_data = full_disease_df,
  vaccination_data = routine_vaccination_data
)

#Take pre-processed case and vaccination data and get it ready for params
case_vaccination_ready <- case_vaccine_to_param(
  demog_data = model_data_preprocessed$processed_demographic_data,
  processed_vaccination = model_data_preprocessed$processed_vaccination_data,
  processed_case = model_data_preprocessed$processed_case_data,
  vaccination_schedule = vaccination_schedule,
  setting = "high"
)

age_vaccination_beta_modifier <- rbind(
  expand.grid(
    dim1 = 1:101,
    dim2 = 2:3,
    dim3 = 1,
    value = 1 - subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("1 dose", description)) %>% pull(value)/100
  ), 
  expand.grid(
    dim1 = 1:101,
    dim2 = 4:5,
    dim3 = 1,
    value = 1 - subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("2 dose", description)) %>% pull(value)/100
  )
)

#Set up model
params <- param_packager(
  
  # Demographic parameters
  n_age = model_data_preprocessed$processed_demographic_data$input_data$n_age,
  n_vacc = model_data_preprocessed$processed_demographic_data$input_data$n_vacc,
  n_risk = model_data_preprocessed$processed_demographic_data$input_data$n_risk,
  
  # Vaccine parameters
  short_term_waning = 1/14 * 365,
  long_term_waning = 1/subset(measles_parameters, parameter == "long_term_waning" & grepl("2 dose", description)) %>% pull(value) * 365,
  age_vaccination_beta_modifier = age_vaccination_beta_modifier,
  
  # Disease parameters - we want to transmission, so set to 0
  R0 = 0,
  
  #Disease parameters
  cfr_normal = 0,
  cfr_severe = (subset(measles_parameters, parameter == "cfr") %>% pull(value) %>% median())/100,
  incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value) * 365,
  recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  severe_recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  
  #Seeding previous cases
  tt_seeded = c(0, 40, 41),#case_vaccination_ready$tt_seeded,
  seeded = rbind(
    data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 1, value = 0),
    data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 100),
    data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 3, value = 0)
  ), #case_vaccination_ready$seeded,
  #Setting up vaccination
  vaccination_coverage = case_vaccination_ready$vaccination_coverage,
  tt_vaccination_coverage = case_vaccination_ready$tt_vaccination,
  
  #Demographic parameters
  contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
  N0 = model_data_preprocessed$processed_demographic_data$N0,
  crude_birth = model_data_preprocessed$processed_demographic_data$crude_birth,
  crude_death = model_data_preprocessed$processed_demographic_data$crude_death,
  simp_birth_death = 0,
  aging_rate = 1,
  tt_migration = model_data_preprocessed$processed_demographic_data$tt_migration,
  migration_in_number = model_data_preprocessed$processed_demographic_data$migration_in_number,
  migration_distribution_values = model_data_preprocessed$processed_demographic_data$migration_distribution_values,

  #Birth ages
  repro_low = 15,
  repro_high = 49,
  
)

#Run model
clean_df <- run_model(
  odin_model = model,
  params = params,
  time = (model_data_preprocessed$processed_demographic_data$input_data$year_end - model_data_preprocessed$processed_demographic_data$input_data$year_start) + 1,
  no_runs = 1
)


#Plot total population
year_start <- model_data_preprocessed$processed_demographic_data$input_data$year_start

#Plot
ggplot(
  data = clean_df %>%
    filter(state == "I" & age == "All"),
  mapping = aes(
    x = time + year_start,
    y = value
  )
) +
  geom_line() +
  labs(
    x = "Year",
    y = "Population"
  ) +
  scale_y_continuous(label = scales::comma) +
  theme_bw()

#Plot
ggplot(
  data = clean_df %>%
    filter(state == "I" & age == "All" & time > 0),
  mapping = aes(
    x = time ,
    y = value
  )
) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Population"
  ) +
  scale_y_continuous(label = scales::comma) +
  theme_bw() +
  facet_wrap(~state, scales = "free_y")



#Plot
vacc_agg <- clean_df %>%
  filter(vaccination != "All" & state %in% c("S", "E", "I", "R", "Is", "Rc")) %>%
  group_by(time, vaccination) %>%
  summarise(
    value = sum(value),
    .groups = "drop"
  ) %>%
  group_by(
    time
  ) %>%
  mutate(coverage = value/sum(value, na.rm = T))


ggplot(
  data = clean_df %>%
    filter(age == "All" & time > 0),
  mapping = aes(
    x = time + year_start,
    y = value
  )
) +
  geom_line() +
  labs(
    x = "Year",
    y = "Population"
  ) +
  scale_y_continuous(label = scales::comma) +
  theme_bw() +
  facet_wrap(~state, scales = "free_y")
