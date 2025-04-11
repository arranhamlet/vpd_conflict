if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  rio,
  here,
  tidyverse,
  reshape2,
  collapse,
  janitor,
  patchwork,
  data.table
)

invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import files
fertility <- import("data/processed/WPP/fertility.csv")
migration <- import("data/processed/WPP/migration.csv")
mortality <- import("data/processed/WPP/deaths.csv")
population_all <- import("data/processed/WPP/age_both.csv")
population_female <- import("data/processed/WPP/age_female.csv")

#Calculate demographics
demographic_data_calculated <- process_demography(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  iso = "ETH",
  n_age = 5,
  n_vac = 1
)

#Disease data
full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")

processed_disease <- process_prior_cases(
  disease_data = full_disease_df,
  disease = "polio",
  iso = demographic_data_calculated$input_data$iso,
  year_start = demographic_data_calculated$input_data$year_start,
  year_end = demographic_data_calculated$input_data$year_end
)

#Vaccination data
full_vaccination_data <- import("data/processed/vaccination/coverage_table.rds")

processed_vaccination <- process_vaccination(
  vaccination_data = full_vaccination_data,
  vaccine = "poliomyelitis",
  iso = demographic_data_calculated$input_data$iso,
  year_start = demographic_data_calculated$input_data$year_start,
  year_end = demographic_data_calculated$input_data$year_end
)

plot_case_vaccination(
  case_data = processed_disease,
  vaccination_data = processed_vaccination
)
