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
  population_modifier = 0.4,
  fertility_modifier = 1.18,
  death_modifier = 1,
  migration_modifier = 1
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

#Run model
clean_df <- run_model(
  params = params,
  time = length(demog_data$tt_migration),
  no_runs = 2
)

#Generate plots
saved_plots <- model_run_demographic_plots(
  clean_df,
  demog_data,
  end_year = 2022
)

saved_plots[[1]]
saved_plots[[2]]
saved_plots[[3]]

#Update with Gaza data on population
pyramid_data <- saved_plots$pyramid_data %>%
  mutate(age_group = factor(c(as.character(saved_plots$pyramid_data$age_group)[1:16], rep("80+", 5)), levels = unique(c(as.character(saved_plots$pyramid_data$age_group)[1:16], rep("80+", 5))))) %>%
  group_by(type, age_group) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(type) %>%
  mutate(prop = value/sum(value))
  

#Load in Gaza specific pyramid
gaza_age_breakdown <- import(here("data", "raw_data", "2022_Gaza_Strip_Age_Structure.csv")) %>%
  clean_names()

#Replace data for a plot
pyramid_data[which(pyramid_data$type == "UN WPP"), ]$prop <- gaza_age_breakdown$median_percent/100
pyramid_data_spread <- pyramid_data %>%
  select(-value) %>%
  spread(key = type, value = prop) %>%
  set_names("age_group", "model_estimate", "un_wpp") %>%
  mutate(absolute_difference = model_estimate - un_wpp,
         relative_difference = (model_estimate - un_wpp)/un_wpp)


#Plot
gaza_demographic <- ggplot(mapping = aes(
  x = age_group,
  fill = type
)) +
  geom_bar(
    data = pyramid_data,
    mapping = aes(y = prop),
    stat = "identity",
    position = position_dodge()
  ) +
  theme_bw() +
  labs(title = paste0("Population proportion by age group (", max(demog_data$years), ")"), 
       y = "",
       x = "",
       fill = "") +
  theme(legend.position = "bottom")

gaza_demographic

gaza_age_prop_difference <- ggplot(mapping = aes(
  x = age_group,
)) +
  geom_bar(
    data = pyramid_data_spread,
    mapping = aes(y = absolute_difference * 100),
    stat = "identity",
    position = position_dodge()
  ) +
  theme_bw() +
  labs(title = paste0("Percent population proportion difference between model\nand Palestinian Statistical Authority estimates by age group (2022)"), 
       y = "",
       x = "",
       fill = "") +
  theme(legend.position = "bottom")



ggsave("figs/demography/gaza_linegraph.jpg", saved_plots[[1]], height = 3, width = 5)
ggsave("figs/demography/gaza_demographic_difference.jpg", gaza_age_prop_difference, height = 3, width = 7)
ggsave("figs/demography/gaza_demographic.jpg", gaza_demographic, height = 3, width = 7)



