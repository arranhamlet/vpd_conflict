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
fertility <- import("data/processed_data/WPP_data/fertility.csv")
migration <- import("data/processed_data/WPP_data/migration.csv")
mortality <- import("data/processed_data/WPP_data/deaths.csv")
population_all <- import("data/processed_data/WPP_data/age_both.csv")
population_female <- import("data/processed_data/WPP_data/age_female.csv")

#Run function - add in modifiers to account for Gaza
time1 <- Sys.time()
demog_data_new <- prepare_demographic_for_model(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  year_start = 1964,
  year_end = "",
  n_age = 2,
  iso3 = "PSE",
  version = "new"
)
time2<- Sys.time()

time3 <- Sys.time()
demog_data_fast <- prepare_demographic_for_model(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  year_start = 1964,
  year_end = "",
  n_age = 2,
  iso3 = "PSE"
)
time4 <- Sys.time()

time2 - time1
time4 - time3


library(microbenchmark)

# Define input test data
# Make sure these are representative of your real-world data
migration <- your_migration_data
fertility <- your_fertility_data
mortality <- your_mortality_data
population_all <- your_population_all_data
population_female <- your_population_female_data

#Calculate demographics
demographic_data_calculated <- prepare_demographic_for_model(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  iso3 = "PSE",
  n_age = 101,
  n_vac = 5
)

#Disease data
disease <- import("data/raw_data/WHO_data/reported_cases_data.xlsx")
disease_clean <- disease %>%
  clean_names() %>%
  filter(group == "COUNTRIES") %>%
  select(-group)

export(disease_clean, "data/processed_data/WHO/reported_case_data.csv")


