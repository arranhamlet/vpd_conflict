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
  ggforce,
  ggpubr,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Run through some countries
countries_of_interest <- c("PSE", "MMR", "SOM", "UKR", "VEN", "GBR")

cases_of_interest <- import("data/processed/WHO/reported_cases_data.csv") %>%
  subset(disease_description == "Measles" & iso3 %in% countries_of_interest) 

population_all <- import(here("data", "processed", "WPP", "age_both.csv")) %>%
  subset(iso3 %in% countries_of_interest)


country_data <- sapply(countries_of_interest, function(a){

  print(a)

  #Run process
  model_data_processed <- data_load_process_wrapper(
    iso = "GBR",
    disease = "measles",
    vaccine = "measles",
    R0 = 12,
    timestep = "day"
  )
  
  new_params <- model_data_processed$params
  new_params$tt_seeded <- c(0, 10)
  new_params$seeded[, , , 2] <- new_params$seeded[, , , 1]
  new_params$seeded[, , , 1] <- 0
  
  new_params$vaccination_coverage <- generate_array_df(dim1 = 101, dim2 = 5, dim3 = 1, dim4 = 3, 
                                                       updates = data.frame(dim1 = 1:101, dim2 = 1, dim3 = 1, dim4 = 2, value = x)) %>%
    df_to_array()
  
  new_params$tt_vaccination_coverage <- c(0, 1, 2)
  new_params$no_vacc_changes <- 3
  
  #Process for plotting
  data_clean <- run_model(
    odin_model = model,
    params = new_params,
    time = 365,
    no_runs = 1
  )

  ggplot(data = data_clean %>% 
           subset(state == "new_case" & age == "All"), 
         mapping = aes(x = time, y = value)) + 
    geom_line() +
    scale_y_continuous(labels = scales::comma)
  
  data_clean %>% 
    subset(state == "I" & age == "All") %>% 
    summarise(value = sum(value))
  

}, simplify = FALSE)
