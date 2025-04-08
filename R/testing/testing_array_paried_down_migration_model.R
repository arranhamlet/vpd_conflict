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

#Import model - massively paired down version that only looks at migration
model <- odin2::odin("models/stochastic_migration_testing.R")

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
  year_start = 2000,
  year_end = ""
)

#Loop this
loop_this <- sapply(c(1, 3, 5), function(n_vacc){
  
  print(n_vacc)
  
  params <- list(
  n_age = 101,
  n_vacc = n_vacc,
  n_risk = 1,
  N0 = generate_array_df(
    dim1 = 101,
    dim2 = n_vacc,
    dim3 = 1,
    updates = demog_data$N0
  ) %>%
    df_to_array,
  no_migration_changes = 24,
  tt_migration = 0:23,
  migration_in_number = generate_array_df(
    dim1 = 24,
    dim2 = 101,
    dim3 = n_vacc,
    dim4 = 1,
    updates = demog_data$migration_in_number
  ) %>%
    df_to_array,
  migration_distribution_values = generate_array_df(
    dim1 = 24,
    dim2 = 6,
    dim3 = 101,
    dim4 = n_vacc,
    dim5 = 1,
    updates = demog_data$migration_distribution_values
  ) %>%
    df_to_array
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

#Migration only
all_looped <- data.table::rbindlist(sapply(loop_this, function(x) x[[1]], simplify = F))
all_params <- sapply(loop_this, function(x) x[[2]], simplify = FALSE)

#Plot
ggplot(
  data = subset(all_looped, run == "run_1" & state == "S" & age == "All"),
  mapping = aes(
    x = time,
    y = value,
    color = as.factor(n_vacc_comp)
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = ""
  )

#Look at different ages
ggplot(
  data = subset(all_looped, run == "run_1" & state == "S" & age != "All"),
  mapping = aes(
    x = as.numeric(age),
    y = value,
    color = as.factor(vaccination)
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  facet_wrap(~n_vacc_comp)




