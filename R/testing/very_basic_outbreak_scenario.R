if(!require("pacman")) install.packages("pacman")
options(scipen = 999)

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
  monty,
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

measles_parameters <- import(here("data", "processed", "model_parameters", "Measles_SEIR_Parameters.csv"))

#Set up model and run
#Run function
demog_data <- process_demography(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  contact_matricies = contact_matricies,
  year_start = "2010",
  year_end = "2011",
  iso = "PSE",
  n_age = 101,
  number_of_vaccines = 1, 
  n_risk = 1
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

#Set up model
params <- param_packager(
  # Demographics
  n_age = demog_data$input_data$n_age,
  n_vacc = demog_data$input_data$n_vacc,
  n_risk = demog_data$input_data$n_risk,
  
  # #Vaccine parameters
  short_term_waning = 1/28,
  long_term_waning = 1/(subset(measles_parameters, parameter == "long_term_waning" & grepl("2 dose", description)) %>% pull(value) * 365),
  
  #Disease parameters
  incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value),
  recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value),

  R0 = 2,
  
  age_vaccination_beta_modifier = age_vaccination_beta_modifier,

  #Infectious
  I0 = 0,
  user_specified_foi = 0,

  tt_seeded = c(0, 10, 11),
  seeded = data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 10),
  
  #Demographic parameters
  contact_matrix = demog_data$contact_matrix,
  N0 = demog_data$N0,
  crude_birth = demog_data$crude_birth[1, ] %>%
    mutate(value = value/365),
  crude_death = demog_data$crude_death %>%
    subset(dim3 == 1) %>%
    mutate(value = value/365),
  aging_rate = 1/365,
  tt_migration = demog_data$tt_migration * 365,
  migration_in_number = demog_data$migration_in_number %>%
    mutate(value = value/365),
  migration_distribution_values = demog_data$migration_distribution_values,
  
  #Birth ages
  repro_low = 15,
  repro_high = 49,
  
)

#Run model
clean_df <- run_model(
  odin_model = model,
  params = params,
  time = 1000,
  no_runs = 4
)

#Subset and aggregate
aggregate_df <- clean_df %>%
  subset(age == "All" & state %in% c("new_case", "S")) %>%
  fgroup_by(state, time) %>%
  fsummarise(value = median(value),
             value_min = quantile(value, 0.025),
             value_max = quantile(value, 0.975))



ggplot() +
  geom_line(data = clean_df %>%
              subset(age == "All" & state %in% c("new_case", "S")),
            mapping = aes(
              x = time,
              y = value,
              color = run
            )) +
  geom_ribbon(data = aggregate_df %>% filter(time > 0),
              mapping = aes(
                x = time,
                y = value,
                ymin = value_min,
                ymax = value_max
              ), alpha = 0.25) +
  facet_wrap(~state, scales = "free_y") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")
