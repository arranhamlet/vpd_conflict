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

#Run function - add in modifiers to account for Gaza
demog_data <- prepare_demographic_for_model(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  year_start = 1964,
  year_end = "",
  population_modifier = 0.293745
)

#Set up model
params <- param_packager(
  
  n_age = 101,
  n_vacc = 1,
  n_risk = 1,
  
  N0 = demog_data$N0,
  
  I0 = 0,
  initial_background_death = 0,
  aging_rate = 1,
  
  #Changing mortality and birth
  #Turn off simple birth/deaths
  simp_birth_death = 0,
  #List of when birth_death_changes
  tt_birth_changes = demog_data$tt_migration,
  tt_death_changes = demog_data$tt_migration,
  #Values of changes
  crude_birth = demog_data$crude_birth,
  crude_death = demog_data$crude_death,
  #Birth ages
  repro_low = 15,
  repro_high = 49,
  
  #Migration
  tt_migration = demog_data$tt_migration,
  migration_in_number = demog_data$migration_in_number,
  migration_distribution_values = 1
  
)

#Set up data
gaza_population <- import("data/raw_data/US_census_data/gaza_demographic_data_1960_2025.csv") %>%
  clean_names()

#Set up population to test the model against
gaza_benchmark <- data.frame(time = gaza_population$year,
                              population_total = gaza_population$total_population/100000) %>%
  subset(time %in% c(1975, 1985, 1995, 2005, 2015, 2023)) %>% mutate(
    time = time - 1964
  )

#Set priors and pack up monty
prior <- monty_dsl({
  fertility_modifier ~ Exponential(mean = 1.1)
  # death_modifier ~ Exponential(mean = 1.05)
})

sir_packer <- monty_packer(c("fertility_modifier"), fixed = within(params, rm(fertility_modifier)))


#Run without stochasticity
unfilter <- dust_unfilter_create(model(), 0, gaza_benchmark)
vcv <- matrix(c(0.0003, 0.0003), 1, 1) * 2
sampler <- monty_sampler_random_walk(vcv)
likelihood_det <- dust_likelihood_monty(unfilter, sir_packer)
posterior_det <- prior + likelihood_det
samples_det <- monty_sample(posterior_det, sampler, 10000,
                            initial = sir_packer$pack(params))

#Gather and plot
ggplot_runs <- t(drop(samples_det$pars)) %>% 
  as.data.frame() %>% 
  gather() %>%
  rownames_to_column(var = "run")

#Plot histogram
ggplot(data = ggplot_runs,
       mapping = aes(fill = key,
                     x = value)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~key) +
  theme(legend.position = "none") +
  labs(x = "Value",
       y = "Count") +
  geom_vline(
    mapping = aes(xintercept = quantile(value, 0.5), linetype = "dashed")
  )

h <- dust_likelihood_last_trajectories(filter)

