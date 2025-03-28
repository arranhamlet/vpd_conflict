if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  rio,
  here,
  tidyverse,
  janitor
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = F), function(x) source(x)))

#Find files
all_files <- list.files(here("data", "raw_data", "WPP_data"), pattern = "WPP2024", full.names = T)

#Run through
sapply(all_files, function(x){
  
  demographic_data_processing(file_name = x,
                              country_subset = "State of Palestine",
                              save = T)
})

