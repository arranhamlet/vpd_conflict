if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  rio,
  here,
  readxl,
  janitor,
  tidyverse
)

#Read in raw data
raw_data <- read_excel(here("data", "raw", "parameters", "gaza_infections_parameters.xlsx"), sheet = 3)

#Create new dataframe
skeleton_df <- data.frame(
  disease = NA,
  age_year = NA,
  age_min_year = NA,
  age_max_year = NA,
  parameter = NA,
  value = NA,
  value_min = NA,
  value_max = NA,
  description = NA
)

processed_df <- Reduce(rbind, sapply(1:nrow(raw_data), function(x){
  
  this_data <- raw_data[x, ]
  
  upd_df <- skeleton_df %>%
    mutate(
      disease = this_data$disease,
      parameter = gsub("_max|_min", "", this_data$parameter),
      age = this_data$age_specific,
      value = if(grepl("maximum", this_data$description) & grepl("minimum", this_data$description)) this_data$value_gen else NA,
      value_max = if(grepl("maximum", this_data$description)) this_data$value_gen else NA,
      value_min = if(grepl("minimum", this_data$description)) this_data$value_gen else NA,
      description = this_data$description
    )
  
  if(is.na(this_data$value_gen)){
    
    upd_df <- Reduce(rbind, sapply(which(grepl("value_a", colnames(this_data))), function(y){
      
      this_data_here <- this_data[, y]
      age <- gsub("value_a", "", names(this_data_here))
      age_split <- unlist(strsplit(age, "to"))
      age_upd <- if(any(grepl("mo", age_split))){
        as.numeric(gsub("mo", "", age_split))/12
      } else {
        as.numeric(gsub("yo", "", age_split))
      }
      
      upd_df %>%
        mutate(
          age_min_year = age_upd[1],
          age_max_year = if(length(age_upd) == 1) age_upd[1] else age_upd[2],
          value = as.numeric(this_data_here)
        )
      
    }, simplify = FALSE))
    
    upd_df
  }
  
  upd_df
  
}, simplify = FALSE))

#Smush the df
processed_df_smush <- processed_df %>%
  group_by(disease, age_min_year, age_max_year, parameter) %>%
  summarise(
    value = median(value, na.rm = T),
    value_min = median(value_min, na.rm = T),
    value_max = median(value_max, na.rm = T),
    .groups = "drop"
  ) %>%
  mutate(
    parameter = gsub("_age_rr", "", parameter),
    parameter = case_when(
      parameter == "pre_tau" ~ "incubation_period",
      parameter == "tau" ~ "infectious_period",
      parameter == "tau_rx" ~ "infectious_period_treated",
      parameter == "pd" ~ "proportion_symptomatic",
      .default = parameter
    )
  )

export(processed_df_smush, file = here("data", "processed", "model_parameters", "gaza_parameters_translated.csv"), na = NA)



