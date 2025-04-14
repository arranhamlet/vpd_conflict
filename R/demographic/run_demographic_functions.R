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

full_vaccination_data <- import("data/processed/vaccination/coverage_table.csv")
full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")

#Run processing
plotted <- model_input_formatter_wrapper(
  iso = "SDN",    
  disease = "measles",
  vaccine = "measles",
  n_age = 101,
  
  #Datasets
  migration,
  fertility,
  mortality,
  population_all,
  population_female,
  disease_data = full_disease_df,
  vaccination_data = full_vaccination_data
)

plotted$demographic_plots



#Test speed
microbenchmark::microbenchmark(
  model_input_formatter_wrapper(
    #Things the user will NEED to specify in the function
    #Iso3 country code
    iso = "PSE",    
    #Disease of interest
    disease = "measles",
    #Vaccine of interest
    vaccine = "measles",
    
    #Things that the user doesnt need to specify, but can
    year_start = "",
    year_end = "",
    n_age = 1,
    n_vacc = 1,
    n_risk = 1,
    population_modifier = 1, 
    fertility_modifier = 1, 
    death_modifier = 1,
    migration_modifier = 1,
    
    #Datasets to be loaded and input - will be inbuilt in the package
    migration,
    fertility,
    mortality,
    population_all,
    population_female,
    disease_data = full_disease_df,
    vaccination_data = full_vaccination_data

  ),
  times = 100
)
