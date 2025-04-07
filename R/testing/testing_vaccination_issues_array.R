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
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = F), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Load files needed
migration <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_gen_f01_demographic_indicators_compact.csv"))
fertility <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_fert_f01_fertility_rates_by_single_age_of_mother.csv"))
mortality <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_mort_f01_1_deaths_single_age_both_sexes.csv"))
population_all <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_pop_f01_1_population_single_age_both_sexes.csv"))
population_female <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_pop_f01_3_population_single_age_female.csv"))

#Loop this
loop_this <- sapply(c(1, 3, 5), function(t){
  
  print(t)
  
  n_vacc = t
  
  #Run function - add in modifiers to account for Gaza
  demog_data <- prepare_demographic_for_model(
    migration = migration, 
    fertility = fertility, 
    mortality = mortality, 
    population_all = population_all, 
    population_female = population_female,
    year_start = 2000,
    year_end = "",
    n_vacc = n_vacc
  )

  #Set up model
  params <- param_packager(
    
    n_age = 101,
    n_vacc = n_vacc,
    n_risk = 1,
    
    N0 = demog_data$N0,

    tt_migration = demog_data$tt_migration,
    migration_in_number = demog_data$migration_in_number,
    migration_distribution_values = demog_data$migration_distribution_values
    
  )

  #Run model
  clean_df <- run_model(
    params = params,
    time = length(demog_data$tt_migration),
    no_runs = 2
  ) %>%
    # filter(run == "run_1") %>%
    mutate(n_vacc_comp = n_vacc)
  
  list(clean_df, params)
  
}, simplify = F)

all_looped <- data.table::rbindlist(sapply(loop_this, function(x) x[[1]], simplify = F))
all_params <- sapply(loop_this, function(x) x[[2]], simplify = FALSE)

#Okay, are the parameters for migration_in_number the same?
#For all columns that are NOT the first vaccination compartment?
sum(all_params[[1]]$migration_in_number[, , -1, ])
sum(all_params[[2]]$migration_in_number[, , -1, ])
sum(all_params[[3]]$migration_in_number[, , -1, ])
#For all the column that IS the first vaccination compartment
sum(all_params[[1]]$migration_in_number[, , 1, ])
sum(all_params[[2]]$migration_in_number[, , 1, ])
sum(all_params[[3]]$migration_in_number[, , 1, ])

#Yep all good - so the values going INTO the model, are the same for each of the different runs of the model
#Now how do the plots look
ggplot(data = subset(all_looped, !grepl("mig|repro", state) & run == "run_1" & age == "All" & time %in% 1:8),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(n_vacc_comp)
       )) +
  facet_wrap(~state, scales = "free") +
  theme_bw() +
  geom_line() +
  scale_y_continuous(label = scales::comma) +
  labs(x = "Time",
       y = "",
       color = "n vacc compartment")

#Okay, so there is a difference in the S, which is leading to a difference in total_pop 

#Now going to aggregate to look at the migration flows.
#Here the parameter migration_in_number is the same as the migout parameter (renamed to spit it back out)
#This variable is then taken to interpolate() over the timestep, and it produces the parameter migint.
#There are no other processes, and so they SHOULD be the same value.
migout_age_collapse <- subset(all_looped, state %in% c("migout", "migint") & run == "run_1" & age != "All" & time %in% 1:8) %>%
  group_by(time, state, vaccination, n_vacc_comp) %>%
  summarise(value = sum(value))

#All the migint values are different, and in fact they are showing that new vaccination compartments are being filled with values, which as we saw in this section:
#For all the column that IS the first vaccination compartment
sum(all_params[[1]]$migration_in_number[, , 1, ])
sum(all_params[[2]]$migration_in_number[, , 1, ])
sum(all_params[[3]]$migration_in_number[, , 1, ])
#They should all be the same value. This is confirmed because all the migout values are the same.

#Plot
ggplot(data = subset(migout_age_collapse),
       mapping = aes(
         x = time,
         y = round(value, 1),
         color = as.factor(vaccination)
       )) +
  facet_wrap(state~n_vacc_comp, scales = "free") +
  theme_bw() +
  geom_line() +
  scale_y_continuous(label = scales::comma) +
  labs(x = "Time",
       y = "",
       color = "n vacc compartment") +
  theme(legend.position = "bottom")

#Total values are also different across migint, but not migout.
#Somehow, during the interpolate section of the model, something is going wrong.
migout_age_collapse %>%
  group_by(state, n_vacc_comp) %>%
  summarise(value = sum(value))

