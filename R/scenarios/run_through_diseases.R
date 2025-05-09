options(scipen = 999)

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

#Run process
GBR_day <- data_load_process_wrapper(
    iso = "GBR",
    disease = "measles",
    vaccine = "measles",
    R0 = 18,
    timestep = "month"
)

GBR_year <- data_load_process_wrapper(
  iso = "GBR",
  disease = "measles",
  vaccine = "measles",
  R0 = 18,
  timestep = "year"
)

#Run model
GBR_day_processed <- run_model(
  odin_model = model,
  params = GBR_day$params,
  time = floor(GBR_day$time),
  no_runs = 1
)

GBR_year_processed <- run_model(
  odin_model = model,
  params = GBR_year$params,
  time = floor(GBR_year$time),
  no_runs = 1
)

GBR_day_processed %>% subset(state == "total_pop" & time == max(time))
GBR_year_processed %>% subset(state == "total_pop" & time == max(time))


sum(GBR_day_processed %>% subset(state == "total_pop") %>% pull(value))
sum(GBR_year_processed %>% subset(state == "total_pop") %>% pull(value))


