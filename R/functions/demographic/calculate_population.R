if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  dust2,
  tidyverse,
  reshape2,
  collapse,
  here,
  rio,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, recursive = T), function(x) source(x)))

#Load in data
fertility <- import(here("data", "raw_data", "palestine_WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.csv"))
mortality_both <- import(here("data", "raw_data", "palestine_WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.csv"))
population_both <- import(here("data", "raw_data", "palestine_WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.csv"))
population_female <- import(here("data", "raw_data", "palestine_WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.csv"))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Yearly changes
#Run for 60 years
time_run_for <- 60 * 365
time <- seq(0, time_run_for, by = 1)
data_change_here <- seq(0, time_run_for, by = 365)
#Work out mortality
mortality_change <- mortality_both %>%
  filter(Year >= 1964) %>%
  select(`0`:`100+`) %>% 
  as.matrix %>%
  c
#Work out births
population_female_matched <- population_female %>%
  filter(Year >= 1964) %>%
  select(`15`:`49`) %>%
  as.matrix

fertility_matched <- fertility %>%
  filter(Year >= 1964) %>%
  select(`15`:`49`) %>%
  as.matrix

population_all <- population_both %>%
  filter(Year >= 1964) %>%
  select(`0`:`100+`) %>% 
  as.matrix %>%
  rowSums()
  c

#Fertility calculations
fertility_by_year <- rowSums(population_female_matched * fertility_matched/100)/population_all



params <- param_packager(
  
  n_age = 101,
  n_vacc = 1,
  n_risk = 1,
  
  N0 = 
  
  I0 = 0,
  initial_background_death = 1/(.16 * 365),
  aging_rate = 1/(365),
  
  #Changing mortality and birth
  #Turn off simple birth/deaths
  simp_birth_death = 0,
  #List of when birth_death_changes
  tt_birth_changes = 0,
  tt_death_changes = 0,
  #Values of changes
  crude_birth = fertility_by_year,
  crude_death = array(mortality_change, dim = c(101, 1, 1)),
  #Birth ages
  repro_low = 15,
  repro_high = 59
)



simp_birth_death = 1,

#Crude birth rate if simp_birth_death != 1
crude_birth = 0,
crude_death = 0,







