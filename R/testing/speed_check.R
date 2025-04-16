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
  data.table,
  microbenchmark
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

speedrun <- microbenchmark::microbenchmark(
  testing_full_runthrough(
    migration, 
    fertility, 
    mortality, 
    population_all, 
    population_female,
    year_start = "1980",
    year_end = "",
    iso = "PSE",
    n_age = 101,
    n_vacc = 5, 
    n_risk = 1
  ),
  testing_full_runthrough(
    migration, 
    fertility, 
    mortality, 
    population_all, 
    population_female,
    year_start = "1980",
    year_end = "",
    iso = "PSE",
    n_age = 101,
    n_vacc = 100, 
    n_risk = 1
  ),
  times = 25
)

speedrun





