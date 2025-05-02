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
contact_matricies <- import(here("data", "raw", "contact_matricies", "contact_all.rdata"))

measles_parameters <- import(here("data", "processed", "model_parameters", "Measles_SEIR_Parameters.csv"))

#Set up model and run
#Run function
demog_data <- process_demography(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  contact_matricies = contact_matricies,
  year_start = "2023",
  year_end = "2023",
  iso = "PSE",
  n_age = 101,
  number_of_vaccines = 1, 
  n_risk = 1
)

age_vaccination_beta_modifier <- rbind(
  expand.grid(
    dim1 = 1:101,
    dim2 = 1,
    dim3 = 1,
    value = 0
  ), 
  expand.grid(
    dim1 = 1:101,
    dim2 = 2:3,
    dim3 = 1,
    value = 0.97
  )
)

#Plot
#Set up model
params <- param_packager(
  # 
  n_age = demog_data$input_data$n_age,
  n_vacc = demog_data$input_data$n_vacc,
  n_risk = demog_data$input_data$n_risk,
  
  #Vaccine parameters
  short_term_waning = 1/28,
  long_term_waning = 0,
  
  #Disease parameters
  incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value),
  recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value),

  R0 = 12,

  #Infectious
  I0 = 0,
  user_specified_foi = 0,
  initial_FOI = 0,
  age_vaccination_beta_modifier = age_vaccination_beta_modifier,
  
  tt_seeded = c(0, 10, 11),
  seeded = data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 1),
  
  #Demographic parameters
  contact_matrix = demog_data$contact_matrix,
  N0 = demog_data$N0,
  # crude_birth = demog_data$crude_birth[1, ] %>%
  #   mutate(value = value/365),
  # crude_death = demog_data$crude_death %>%
  #   subset(dim3 == 1) %>%
  #   mutate(value = value/365),
  simp_birth_death = 0,
  # aging_rate = 1/365,

  #Birth ages
  repro_low = 15,
  repro_high = 49,
  
)

#Run model
clean_df <- run_model(
  odin_model = model,
  params = params,
  time = 250,#364 * 5,#(demog_data$input_data$year_end - demog_data$input_data$year_start) + 1,
  no_runs = 1
)

ggplot(
  data = clean_df %>%
    subset(state == "I" & age == "All"),
  mapping = aes(
    x = time,
    y = value
  )
) +
  geom_line() +
  theme_bw() +
  scale_y_continuous()

#Loop loads
prob_out <- sapply(seq(0, 100, by = 10), function(x){
  
  print(x)
  
  params_upd <- params
  vacc_upd <- abind::abind(params_upd$vaccination_coverage, 
                           params_upd$vaccination_coverage, 
                           params_upd$vaccination_coverage, along = 4)
  vacc_upd[, 1, , 2] <- x/100
  params_upd$vaccination_coverage <- vacc_upd
  params_upd$tt_vaccination_coverage <- c(0, 1, 2)
  params_upd$no_vacc_changes <- 3
  
  #Run model
  clean_df <- run_model(
    odin_model = model,
    params = params_upd,
    time = 750,
    no_runs = 10
  ) %>%
    filter(state %in% c("S", "new_case") &
             vaccination != "All")
  
  values <- clean_df %>%
    fgroup_by(time, state, vaccination) %>%
    fsummarise(value = median(value),
               value_low = quantile(value, 0.025),
               value_high = quantile(value, 0.975)) %>%
    mutate(vac_coverage = x)
  
  cases_total <- clean_df %>%
    subset(state == "new_case") %>%
    group_by(run) %>%
    summarise(value = sum(value))
  
  values %>%
    mutate(above_100 = sum(cases_total$value >= 100)/nrow(cases_total))
  
}, simplify = FALSE)

combo <- Reduce(rbind, prob_out) %>%
  fgroup_by(time, state, above_100, vac_coverage) %>%
  fsummarise(
    value = sum(value),
    value_low = sum(value_low),
    value_high = sum(value_high)
  )
  
#Plot
ggplot(
  data = combo %>% 
    subset(state == "new_case"),
  mapping = aes(
    x = time,
    y = value,
    ymin = value_low,
    ymax = value_high,
    group = vac_coverage,
    color = as.factor(vac_coverage),
    fill = as.factor(vac_coverage)
  )
) +
  geom_line() +
  geom_ribbon()


ggplot(data = combo %>%
         subset(state == "new_case" & time == 1),
       mapping = aes(
         x = vac_coverage,
         y = above_100
       )) +
  geom_bar(stat = "identity")


