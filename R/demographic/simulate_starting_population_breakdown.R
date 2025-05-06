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
sia_vaccination <- import("data/processed/vaccination/sia_vimc.rds")
VIMC_vaccination <- import("C:/Users/ah1114/Documents/Imperial/VPD_conflict/generic_vpd_models/data/processed/vaccination/coverage_table.rds")

measles_cases <- import(here("data", "raw", "cases", "UK_measles_cases_1940_2023.xlsx")) %>%
  clean_names() %>%
  select(1:3)

#Run processing
model_data_preprocessed <- model_input_formatter_wrapper(
  iso = "GBR",    
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
  vaccination_data_routine = routine_vaccination_data,
  vaccination_data_sia = sia_vaccination,
  VIMC_vaccination = VIMC_vaccination,
  year_start = 1950
)

#Take pre-processed case and vaccination data and get it ready for params
# case_vaccination_ready <- case_vaccine_to_param_vimc(
#   demog_data = model_data_preprocessed$processed_demographic_data,
#   processed_vaccination_vimc = model_data_preprocessed$processed_vaccination_vimc,
#   processed_case = model_data_preprocessed$processed_case_data,
#   vaccination_schedule = vaccination_schedule,
#   setting = "high"
# )

case_vaccination_ready <- case_vaccine_to_param(
  demog_data = model_data_preprocessed$processed_demographic_data,
  processed_vaccination = model_data_preprocessed$processed_vaccination_data,
  processed_vaccination_sia = model_data_preprocessed$processed_vaccination_sia,
  processed_case = model_data_preprocessed$processed_case_data,
  vaccination_schedule = vaccination_schedule,
  setting = "high"
)

age_vaccination_beta_modifier <- rbind(
  expand.grid(
    dim1 = 1:101,
    dim2 = 2:3,
    dim3 = 1,
    value = 1#subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("1 dose", description)) %>% pull(value)/100
  ),
  expand.grid(
    dim1 = 1:101,
    dim2 = 4:5,
    dim3 = 1,
    value = subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("2 dose", description)) %>% pull(value)/100
  )
)

#Okay we are going to be using a constant FOI
initial_FOI <- calculate_foi_from_R0(
  R0 = 12,
  contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
  S = model_data_preprocessed$processed_demographic_data$N0[, 4],
  infectious_period = subset(measles_parameters, parameter == "recovery_rate") %>% pull(value)
)

#Set up model
params <- param_packager(
  
  # Demographic parameters
  n_age = model_data_preprocessed$processed_demographic_data$input_data$n_age,
  n_vacc = model_data_preprocessed$processed_demographic_data$input_data$n_vacc,
  n_risk = model_data_preprocessed$processed_demographic_data$input_data$n_risk,
  
  # Vaccine parameters
  short_term_waning = 1/(14/365),
  long_term_waning = 0,#1/subset(measles_parameters, parameter == "long_term_waning" & grepl("2 dose", description)) %>% pull(value),
  age_vaccination_beta_modifier = age_vaccination_beta_modifier,
  
  # Disease parameters 
  R0 = 12,
  user_specified_foi = 1,
  initial_FOI = initial_FOI,

  #Disease parameters
  cfr_normal = 0,
  cfr_severe = (subset(measles_parameters, parameter == "cfr") %>% pull(value) %>% median())/100,
  incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value) * 365,
  recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  severe_recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  
  #Seeding previous cases
  foi_turn_off_when_vaccinating = 0,
  I0 = 0,
  # tt_seeded = case_vaccination_ready$tt_seeded,
  # seeded = case_vaccination_ready$seeded,
  #Setting up vaccination
  vaccination_coverage = case_vaccination_ready$vaccination_coverage,
  tt_vaccination_coverage = case_vaccination_ready$tt_vaccination, 

  #Demographic parameters
  contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
  N0 = model_data_preprocessed$processed_demographic_data$N0,
  #List of when birth_death_changes
  tt_birth_changes = model_data_preprocessed$processed_demographic_data$tt_migration,
  tt_death_changes = model_data_preprocessed$processed_demographic_data$tt_migration,
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
  age_maternal_protection_ends = 1,
  protection_weight_vacc = 1,
  protection_weight_rec = 1,
  migration_represent_current_pop = 1

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
    filter(state %in% c("S", "E", "I", "R", "Is", "Rc") & age == "All" & time > 5),
  mapping = aes(
    x = time + year_start,
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
ggplot(
  data = clean_df %>%
    filter(state %in% c("I") & age == "All" & time > 50),
  mapping = aes(
    x = time + year_start,
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


#For select ages
vacc_age <- subset(clean_df, state %in% c("S", "E", "I", "R", "Is", "Rc")) %>%
  ungroup() %>%
  group_by(time, age, vaccination) %>%
  summarise(value = sum(value)) %>%
  mutate(
    coverage = value/sum(value, na.rm = T),
    coverage = case_when(
      is.nan(coverage) ~ 0,
      !is.nan(coverage) ~ coverage
    )
  )

vaccine_by_age <- ggplot(
  data = vacc_age %>%
    subset(time == max(time) &
             age != "All") %>%
    mutate(vaccination = case_when(
      vaccination == 1 ~ "Unvaccinated",
      vaccination %in% 2:3 ~ "1 dose",
      vaccination %in% 4:5 ~ "2 doses",
    ),
    vaccination = factor(vaccination, levels = c("Unvaccinated", "1 dose", "2 doses"))),
  mapping = aes(
    x = as.numeric(age),
    y = value,
    fill = vaccination
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    x = "Age",
    y = "Population",
    fill = "",
    title = "Measles vaccination coverage (2024)"
  ) +
  scale_y_continuous(labels = scales::comma)


# ggplot(
#   data = subset(clean_df, state == "Reff_age"),
#   mapping = aes(
#     x = time,
#     y = as.numeric(age),
#     fill = value
#   )
# ) +
#   geom_tile()


#Okay more complicated plot, combine vaccination and R to create graph of those immune
susceptibility_data <- subset(clean_df, state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All") %>%
  mutate(
    status = case_when(
      state == "S" & vaccination == 1 ~ "Susceptible",
      state == "S" & vaccination > 1 ~ "Vaccine protected",
      state != "S" & vaccination == 1 ~ "Exposure protected",
      state != "S" & vaccination > 1 ~ "Vaccine and exposure protected"
      
    ),
    status = factor(status, levels = c("Susceptible", "Vaccine protected", "Exposure protected", 
                                       "Vaccine and exposure protected"))
  ) %>%
  group_by(
    time, age, status
  ) %>%
  summarise(
    value = sum(value),
    .groups = "keep"
  ) %>%
  mutate(
    coverage = value/sum(value, na.rm = T),
    coverage = case_when(
      is.nan(coverage) ~ 0,
      !is.nan(coverage) ~ coverage
    )
  )

protection_by_age <- ggplot(
  data = susceptibility_data %>%
    subset(time == max(time)),
  mapping = aes(
    x = as.numeric(age),
    y = value,
    fill = status
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    x = "Age",
    y = "Population",
    fill = "",
    title = "Measles susceptibility (2024)"
  ) +
  scale_y_continuous(labels = scales::comma)

vaccine_by_age / protection_by_age


#Overall protection in the population
pop_prot <- susceptibility_data %>%
  mutate(
    status_simple = case_when(
      status == "Susceptible" ~ "Susceptible",
      status != "Susceptible" ~ "Protected"
    )
  ) %>%
  group_by(time, status_simple) %>%
  summarise(
    value = sum(value)
  ) %>%
  group_by(time) %>%
  mutate(
    prop = value/sum(value)
  )


ggplot(data = pop_prot %>%
         mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))) %>%
         subset(time >= 1),
       mapping = aes(
         x = time + year_start,
         y = prop * 100,
         fill = status_simple
       )) +
  geom_bar(width = 1, stat = "identity") +
  theme_bw() +
  labs(
    y = "Percent of the population",
    x = "Year" ,
    fill = ""
  )

ggplot(data = pop_prot %>%
         mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))) %>%
         subset(status_simple == "Protected"),
       mapping = aes(
         x = time + year_start,
         y = prop * 100,
         fill = status_simple
       )) +
  geom_line() +
  theme_bw() +
  labs(
    y = "Population level vaccination coverage (%)",
    x = "Year" ,
    fill = ""
  )
