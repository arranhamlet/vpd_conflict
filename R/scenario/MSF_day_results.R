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
  ggpubr,
  patchwork,
  squire.page,
  patchwork,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Countries of interest
countries_interest <- c("SYR", "PSE", "YEM", "NGA", "ETH", "SDN", "MMR", "PNG", "AFG", "VEN", "HTI", "GTM")

#Load WHO disease data
cases_of_interest <- import("data/processed/WHO/reported_cases_data.csv") %>%
  subset(disease_description %in% c("Pertussis", "Measles", "Diphtheria") & iso3 %in% countries_interest)  %>%
  mutate(disease_description = tolower(disease_description))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Run through diseases of interest
diseases <- data.frame(
  disease = c("measles", "pertussis", "diphtheria"),
  R0 = c(15, 12, 3)
)

#Loop
all_run <- sapply(1:nrow(diseases), function(x){
  
  print(x)
  
  #Run process
  model_data_processed <- data_load_process_wrapper(
    iso = "GBR",
    disease = diseases$disease[x],
    vaccine = diseases$disease[x],
    R0 = diseases$R0[x],
    timestep = "week",
    WHO_seed_switch = T
  )
  
  #Process for plotting
  model_ran <- run_model(
    odin_model = model,
    params = model_data_processed$params,
    time = floor(model_data_processed$time),
    no_runs = 2
  )
  
  process_for_plotting(model_ran, input_data = model_data_processed$input_data)
  
}, simplify = FALSE)




