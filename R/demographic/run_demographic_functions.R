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
  scales,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import files
fertility <- import("data/processed/WPP/fertility.csv")
migration <- import("data/processed/WPP/migration.csv")
mortality <- import("data/processed/WPP/deaths.csv")
population_all <- import("data/processed/WPP/age_both.csv")
population_female <- import("data/processed/WPP/age_female.csv")
full_vaccination_data <- import("data/processed/vaccination/coverage_table.rds")
full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")

#Run processing
model_input_formatter_wrapper(
  #Datasets to be loaded and input
  migration,
  fertility,
  mortality,
  population_all,
  population_female,
  #Iso3 country code
  iso = "GBR",
  #WHO disease data
  disease_data = full_disease_df,
  #Disease of interest
  disease = "measles",
  #Pre-calculated vaccination data
  vaccination_data = full_vaccination_data,
  #Vaccine of interest
  vaccine = "measles",
  
  year_start = "",
  year_end = "",
  n_age = 1,
  n_vacc = 1,
  n_risk = 1
)
