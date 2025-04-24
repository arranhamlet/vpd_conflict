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
loop_this <- sapply(c(0, 1), function(t){
  
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
    year_end = "2015",
    iso = "ETH",
    n_age = 101,
    number_of_vaccines = n_vacc, 
    n_risk = n_risk
  )

  #Set up model
  params <- param_packager(
    
    n_age = demog_data$input_data$n_age,
    n_vacc = demog_data$input_data$n_vacc,
    n_risk = demog_data$input_data$n_risk,
    short_term_waning = 1/14,
    long_term_waning = 1/140,
    
    N0 = demog_data$N0,
    crude_birth = demog_data$crude_birth %>%
      mutate(value = value/365),
    crude_death = demog_data$crude_death %>%
      mutate(value = value/365),
    simp_birth_death = 0,
    aging_rate = 0,
    
    tt_migration = demog_data$tt_migration * 365,
    migration_in_number = demog_data$migration_in_number %>%
      mutate(value = value/365),
    migration_distribution_values = demog_data$migration_distribution_values,
    
    #Birth ages
    repro_low = 15,
    repro_high = 49,
    
    tt_vaccination_coverage = c(0, 50, 51),
    vaccination_coverage = 
      data.frame(
        dim1 = 1:101,
        dim2 = 1,
        dim3 = 1,
        dim4 = 2,
        value = 1
      )
  )

  #Run model
  clean_df <- run_model(
    odin_model = model,
    params = params,
    time = 364,#(demog_data$input_data$year_end - demog_data$input_data$year_start) + 1,
    no_runs = 1
  ) %>%
    mutate(n_vacc_comp = t)
  
  list(clean_df, params)
  
}, simplify = F)

all_looped <- data.table::rbindlist(sapply(loop_this, function(x) x[[1]], simplify = F))
all_params <- sapply(loop_this, function(x) x[[2]], simplify = FALSE)

#Plot overall states
ggplot(data = subset(all_looped, age == "All" & state %in% c("S", "E", "I", "R", "Is", "Rc")),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(n_vacc_comp)
       )) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  scale_y_continuous(label = scales::comma)

#Plot overall states
ggplot(data = subset(all_looped, age == "All" & !state %in% c("S", "E", "I", "R", "Is", "Rc") ),
       mapping = aes(
         x = time,
         y = round(value, 1),
         color = as.factor(n_vacc_comp)
       )) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  scale_y_continuous(label = scales::comma)

#Plot vaccination
vaccination_data <- all_looped %>%
  filter(vaccination != "All" & state == "S") %>%
  group_by(time, vaccination, n_vacc_comp) %>%
  summarise(
    value = sum(value)
  ) %>%
  group_by(time, n_vacc_comp) %>%
  mutate(percent = value/sum(value))

ggplot(data = vaccination_data %>%
         filter(n_vacc_comp == 1) %>%
         mutate(vaccination = case_when(
           vaccination == 1 ~ "No vaccine protection",
           vaccination == 2 ~ "Long term protection",
           vaccination == 3 ~ "Short term protection"
         ),
         vaccination = factor(vaccination, levels = c(
           "No vaccine protection",
           "Long term protection",
           "Short term protection"
         ))),
       mapping = aes(
         x = time,
         y = 100 * percent,
         color = as.factor(vaccination)
       )) +
  geom_line() +
  facet_wrap(~vaccination) +
  scale_y_continuous(label = scales::comma) +
  theme_bw() +
  labs(x = "Time",
       y = "Percent of population",
       color = "") 

#Plot vaccination
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








