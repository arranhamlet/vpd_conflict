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

#Loop this
loop_this <- sapply(c(1, 100), function(t){
  
  print(t)
  
  n_vacc = t
  n_risk = 1

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
    n_age = 101,
    n_vacc = n_vacc, 
    n_risk = n_risk
  )

  #Set up model
  params <- param_packager(
    
    n_age = 101,
    n_vacc = n_vacc,
    n_risk = n_risk,
    
    N0 = demog_data$N0,
    crude_birth = demog_data$crude_birth,
    crude_death = demog_data$crude_death,
    simp_birth_death = 0,
    aging_rate = 1,
    
    tt_migration = demog_data$tt_migration,
    migration_in_number = demog_data$migration_in_number,
    migration_distribution_values = demog_data$migration_distribution_values
    
  )

  #Run model
  clean_df <- run_model(
    odin_model = model,
    params = params,
    time = (demog_data$input_data$year_end - demog_data$input_data$year_start) + 1,
    no_runs = 2
  ) %>%
    mutate(n_vacc_comp = t)
  
  list(clean_df, params)
  
}, simplify = F)

all_looped <- data.table::rbindlist(sapply(loop_this, function(x) x[[1]], simplify = F))
all_params <- sapply(loop_this, function(x) x[[2]], simplify = FALSE)

#Plot overall states
ggplot(data = subset(all_looped, age == "All" & run == "run_1" & state %in% c("S", "E", "I", "R", "Is", "Rc")),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(n_vacc_comp)
       )) +
  geom_line() +
  facet_wrap(~state, scales = "free_y")

#Plot overall states
ggplot(data = subset(all_looped, age == "All" & run == "run_1" & !state %in% c("S", "E", "I", "R", "Is", "Rc")),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(n_vacc_comp)
       )) +
  geom_line() +
  facet_wrap(~state, scales = "free_y")


time_S <- subset(all_looped, age == "All" & state == "S")

aggregate_df <- time_S %>%
  fgroup_by(state, time, n_vacc_comp) %>%
  fsummarise(value = median(value),
             value_high = quantile(value, 0.975),
             value_low = quantile(value, 0.025))

#Overall plot
ggplot(data = aggregate_df,
       mapping = aes(
         x = time,
         y = value,
         ymin = value_low,
         ymax = value_high,
         color = as.factor(n_vacc_comp),
         fill = as.factor(n_vacc_comp)
       )) +
  theme_bw() +
  geom_line() +
  geom_ribbon(alpha = 0.25) + 
  scale_y_continuous(label = scales::comma) +
  labs(x = "Time",
       y = "",
       color = "n vacc compartment",
       fill = "n vacc compartment")
  
#Check by age
ggplot(
  data = subset(all_looped, state == "S" & vaccination == 1 & age %in% c(0, 18, 25, 50, 75) & run == "run_1" & age != "All"),
  mapping = aes(
    x = time,
    y = value,
    color = as.factor(n_vacc_comp)
  )) +
  facet_wrap(~age) +
  geom_line()



subset(all_looped, age == "All" & state %in% c("full_migration", "manual_migration") & run == "run_1") %>%
  spread(value = value, key = state) %>%
  mutate(migration_difference = full_migration - manual_migration)




