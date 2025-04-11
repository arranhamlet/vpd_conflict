if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  rio,
  here,
  tidyverse,
  reshape2,
  collapse,
  janitor,
  data.table
)

invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = F), function(x) source(x)))

#Import files
fertility <- import("data/processed_data/WPP/fertility.csv")
migration <- import("data/processed_data/WPP/migration.csv")
mortality <- import("data/processed_data/WPP/deaths.csv")
population_all <- import("data/processed_data/WPP/age_both.csv")
population_female <- import("data/processed_data/WPP/age_female.csv")

#Calculate demographics
demographic_data_calculated <- prepare_demographic_for_model(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  iso3 = "ETH",
  n_age = 5,
  n_vac = 1
)

#Disease data
full_disease_df <- import("data/processed_data/WHO/reported_cases_data.csv")

processed_disease <- disease_processor(
  disease_data = full_disease_df,
  iso3 = demographic_data_calculated$input_data$iso3,
  year_start = demographic_data_calculated$input_data$year_start,
  year_end = demographic_data_calculated$input_data$year_end
)



