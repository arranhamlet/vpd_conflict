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
  data.table
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

#Run function - add in modifiers to account for Gaza
demog_data <- process_demography(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  year_start = "1985",
  year_end = "1995",
  iso = "PSE",
  n_age = 1,
  number_of_vaccines = 3, 
  n_risk = 1
)

#Set up vaccination
tt_vaccination_coverage <- c(0, 1, 2, 9, 10, 11)
coverage <- c(0, 0.5, 0, .25, .5, 0)

vaccination_coverage <- sapply(1:length(tt_vaccination_coverage), function(x){
  
  data.frame(dim1 = 1:101, dim2 = )
  
}, simplify = FALSE)


#Set up model
params <- param_packager(
  
  n_age = demog_data$input_data$n_age,
  n_vacc = demog_data$input_data$n_vacc,
  n_risk = demog_data$input_data$n_risk,
  
  N0 = demog_data$N0,
  crude_birth = demog_data$crude_birth,
  crude_death = demog_data$crude_death,
  simp_birth_death = 0,
  aging_rate = 1,
  
  tt_migration = demog_data$tt_migration,
  migration_in_number = demog_data$migration_in_number,
  migration_distribution_values = demog_data$migration_distribution_values,
  
  #Vaccination
  vaccination_coverage = ,
  tt_vaccination_coverage = 0,
  age_vaccination_beta_modifier = 0,
  #Vaccination waning
  short_term_waning = 0,
  long_term_waning = 0,
  
)

#Run model
clean_df <- run_model(
  odin_model = model,
  params = params,
  time = (demog_data$input_data$year_end - demog_data$input_data$year_start) + 1,
  no_runs = 1
) %>%
  mutate(n_vacc_comp = demog_data$input_data$n_vacc)



#Plot overall states
ggplot(data = subset(clean_df, age == "All" & state %in% c("S", "E", "I", "R", "Is", "Rc")),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(n_vacc_comp)
       )) +
  geom_line() +
  facet_wrap(~state, scales = "free_y")

clean_df_vacc_agg <- clean_df %>%
  filter(state == "S") %>%
  fgroup_by(state, time, vaccination) %>%
  fsummarise(value = median(value),
             value_high = quantile(value, 0.975),
             value_low = quantile(value, 0.025))

ggplot(data = clean_df_vacc_agg,
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(vaccination)
       )) +
  geom_line() +
  facet_wrap(~vaccination, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none")
