if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  dust2,
  tidyverse,
  reshape2,
  collapse,
  here,
  rio,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, recursive = T), function(x) source(x)))

#Load in data
fertility <- import(here("data", "raw_data", "palestine_WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.csv"))
mortality_both <- import(here("data", "raw_data", "palestine_WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.csv"))
population_both <- import(here("data", "raw_data", "palestine_WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.csv"))
population_female <- import(here("data", "raw_data", "palestine_WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.csv"))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

population_subset <- population_both %>%
  filter(Year >= 1964) %>%
  select(`0`:`100+`) 

#Yearly changes
#Run for 60 years
time_run_for <- 60 * 365
time_all <- seq(0, time_run_for, by = 1)
data_change_here <- seq(0, time_run_for, by = 365)
#Work out mortality
mortality_change <- mortality_both %>%
  filter(Year >= 1964) %>%
  select(`0`:`100+`) 

mortality_correct_format <- mortality_change/population_subset
is.na(mortality_correct_format)<-sapply(mortality_correct_format, is.infinite)
mortality_correct_format[is.na(mortality_correct_format)]<-1
mortality_vector <- mortality_correct_format %>%
  as.matrix %>%
  c
mortality_vector <- pmin(mortality_vector, 1)


#Work out births
population_female_matched <- population_female %>%
  filter(Year >= 1964) %>%
  select(`15`:`49`) %>%
  as.matrix

fertility_matched <- fertility %>%
  filter(Year >= 1964) %>%
  select(`15`:`49`) %>%
  as.matrix

population_all <- population_subset %>% 
  as.matrix %>%
  rowSums() %>%
  c

#Fertility calculations
fertility_by_year <- rowSums(population_female_matched * fertility_matched/100)/population_all

params <- param_packager(
  
  n_age = 101,
  n_vacc = 1,
  n_risk = 1,
  
  N0 = array(c(unlist(round(population_subset[1, ] * 1000, 0))), dim = c(101, 1, 1)),
  
  I0 = 0,
  initial_background_death = 1/(.16 * 365),
  aging_rate = 1/(365),
  
  #Changing mortality and birth
  #Turn off simple birth/deaths
  simp_birth_death = 0,
  #List of when birth_death_changes
  tt_birth_changes = data_change_here,
  tt_death_changes = data_change_here,
  #Values of changes
  crude_birth = array(fertility_by_year, dim = c(length(data_change_here), 1)),
  crude_death = array(mortality_vector, dim = c(length(data_change_here), 101, 1)),
  #Birth ages
  repro_low = 15,
  repro_high = 59
)


#Run model
time1 <- Sys.time()
clean_df <- run_model(
  params = params,
  time = time_run_for,
  no_runs = 1
)
time2 <- Sys.time()

time2 - time1


ggplot(
  data = subset(clean_df, state == "total_pop"),
  mapping = aes(
    x = time,
    y = value
  )
) +
  geom_line()

