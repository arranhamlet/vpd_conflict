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
  year_start = "2010",
  year_end = "2020",
  iso = "PSE",
  n_age = 101,
  number_of_vaccines = 0, 
  n_risk = 1
)

#Set up CFR
cfr <- 0:100
cfr[1:5] <- subset(measles_parameters, parameter == "cfr" & grepl("0-4", description)) %>% pull(value)/100
cfr[6:18] <- subset(measles_parameters, parameter == "cfr" & grepl("5-17", description)) %>% pull(value)/100
cfr[19:65] <- subset(measles_parameters, parameter == "cfr" & grepl("18-64", description)) %>% pull(value)/100
cfr[66:101] <- subset(measles_parameters, parameter == "cfr" & grepl("65+", description)) %>% pull(value)/100
#Set up prop_severe
prop_severe <- 0:100
prop_severe[1:5] <- subset(measles_parameters, parameter == "prop_severe" & grepl("0-4", description)) %>% pull(value)/100
prop_severe[6:18] <- subset(measles_parameters, parameter == "prop_severe" & grepl("5-17", description)) %>% pull(value)/100
prop_severe[19:65] <- subset(measles_parameters, parameter == "prop_severe" & grepl("18-64", description)) %>% pull(value)/100
prop_severe[66:101] <- subset(measles_parameters, parameter == "prop_severe" & grepl("65+", description)) %>% pull(value)/100
#Set up prop_complications
prop_complications <- 0:100
prop_complications[1:5] <- subset(measles_parameters, parameter == "prop_complications" & grepl("0-4", description)) %>% pull(value)/100
prop_complications[6:18] <- subset(measles_parameters, parameter == "prop_complications" & grepl("5-17", description)) %>% pull(value)/100
prop_complications[19:65] <- subset(measles_parameters, parameter == "prop_complications" & grepl("18-64", description)) %>% pull(value)/100
prop_complications[66:101] <- subset(measles_parameters, parameter == "prop_complications" & grepl("65+", description)) %>% pull(value)/100

age_vaccination_beta_modifier <- rbind(
  expand.grid(
    dim1 = 1:101,
    dim2 = 2:3,
    dim3 = 1,
    value = 1 - subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("1 dose", description)) %>% pull(value)/100
  ), 
  expand.grid(
    dim1 = 1:101,
    dim2 = 4:5,
    dim3 = 1,
    value = 1 - subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("2 dose", description)) %>% pull(value)/100
  )
)

#Initial FOI
initial_FOI <- calculate_foi_from_R0(
  R0 = 2,
  contact_matrix = demog_data$contact_matrix,
  S = demog_data$N0[, 4]
)

#Set up model
params <- param_packager(
  # 
  n_age = demog_data$input_data$n_age,
  n_vacc = demog_data$input_data$n_vacc,
  n_risk = demog_data$input_data$n_risk,
  # 
  # #Vaccine parameters
  short_term_waning = 1/14,
  long_term_waning = 1/(subset(measles_parameters, parameter == "long_term_waning" & grepl("2 dose", description)) %>% pull(value) * 365),
  # 
  #Disease parameters
  cfr_normal = 0,#median(cfr),
  cfr_severe = 0,#median(cfr),
  incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value),
  recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value),
  severe_recovery_rate = 0,#1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value),
  
  prop_complications = 0,#median(prop_complications),
  prop_severe = 0,#median(prop_severe),
  R0 = 2,
  # age_vaccination_beta_modifier = age_vaccination_beta_modifier,
  natural_immunity_waning = 0,
  initial_background_death = 0,
  
  #Infectious
  I0 = 0,
  user_specified_foi = 1,
  initial_FOI = initial_FOI,
  
  # tt_seeded = c(0, 10, 11),
  # seeded = data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 10),
  
  #Demographic parameters
  contact_matrix = demog_data$contact_matrix,
  N0 = demog_data$N0,
  crude_birth = demog_data$crude_birth[1, ] %>%
    mutate(value = value/365),
  crude_death = demog_data$crude_death %>%
    subset(dim3 == 1) %>%
    mutate(value = value/365),
  simp_birth_death = 0,
  aging_rate = 1/365,
  tt_migration = demog_data$tt_migration * 365,
  migration_in_number = demog_data$migration_in_number %>%
    mutate(value = value/365),
  migration_distribution_values = demog_data$migration_distribution_values,
  #
  #Birth ages
  # repro_low = 15,
  # repro_high = 49,
  
)

#Run model
clean_df <- run_model(
  odin_model = model,
  params = params,
  time = 3000,#364 * 5,#(demog_data$input_data$year_end - demog_data$input_data$year_start) + 1,
  no_runs = 4
)

#Subset and aggregate
aggregate_df <- clean_df %>%
  subset(age == "All") %>%
  fgroup_by(state, time) %>%
  fsummarise(value = median(value),
             value_min = quantile(value, 0.025),
             value_max = quantile(value, 0.975))


ggplot(data = aggregate_df %>% filter(time > 0),
       mapping = aes(
         x = time,
         y = value,
         ymin = value_min,
         ymax = value_max
       )) +
  geom_line() +
  geom_ribbon(alpha = 0.25) +
  facet_wrap(~state, scales = "free_y") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")


#Age prop matrix
reff_age_sum <- clean_df %>% 
  subset(time > 0 & age != "All" & vaccination == "All" & risk == "All" & state %in% c("Reff_age")) %>%
  fgroup_by(state, time, age) %>%
  fsummarise(value = median(value))

categorize_age <- function(age_vector) {
  # Define breaks and labels up to 100+
  breaks <- c(seq(0, 100, by = 5), Inf)
  labels <- c(paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-"), "100+")
  
  # Cut ages into categories
  age_cat <- cut(age_vector, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)
  
  return(age_cat)
}


reff_age_agg <- reff_age_sum %>%
  mutate(age_cat = categorize_age(as.numeric(age))) %>%
  fgroup_by(time, age_cat) %>%
  fsummarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(
    prop = value/sum(value),
    prop = ifelse(is.nan(prop), 0, prop)
  )
  

#Plot heatmap of whats going on
ggplot(data = reff_age_agg,
       mapping = aes(
         x = time,
         y = age_cat,
         fill = prop
       )) +
  geom_tile() +
  theme_bw() +
  labs(x = "Age",
       y = "Time") +
  scale_fill_viridis_c()


ggplot(data = reff_age_agg,
       mapping = aes(
         x = time,
         y = age_cat,
         fill = value/5
       )) +
  geom_tile() +
  theme_bw() +
  labs(x = "Age",
       y = "Time") +
  scale_fill_viridis_c()
