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
  patchwork,
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

routine_vaccination_data <- import("data/raw/WHO/coverage-data_updated.xlsx")
full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")
vaccination_schedule <- import("data/processed/WHO/WHO_vaccination_schedule.xlsx")
measles_parameters <- import(here("data", "processed", "model_parameters", "Measles_SEIR_Parameters.csv"))
sia_vaccination <- import("data/processed/vaccination/sia_vimc.rds")
VIMC_vaccination <- import("C:/Users/ah1114/Documents/Imperial/VPD_conflict/generic_vpd_models/data/processed/vaccination/coverage_table.rds")

measles_cases <- import(here("data", "raw", "cases", "UK_measles_cases_1940_2023.xlsx")) %>%
  clean_names() %>%
  select(1:3)

#Run processing
model_data_preprocessed <- model_input_formatter_wrapper(
  iso = "PSE",    
  disease = "measles",
  vaccine = "measles",
  n_age = 101,
  number_of_vaccines = 2,
  #Datasets
  migration,
  fertility,
  mortality,
  population_all,
  population_female,
  contact_matricies = contact_matricies,
  disease_data = full_disease_df,
  vaccination_data_routine = routine_vaccination_data,
  vaccination_data_sia = sia_vaccination,
  VIMC_vaccination = VIMC_vaccination,
  year_start = 1970,
  year_end = 2023
)

#Take pre-processed case and vaccination data and get it ready for params
case_vaccination_ready <- case_vaccine_to_param_vimc(
  demog_data = model_data_preprocessed$processed_demographic_data,
  processed_vaccination_vimc = model_data_preprocessed$processed_vaccination_vimc,
  processed_case = model_data_preprocessed$processed_case_data,
  vaccination_schedule = vaccination_schedule,
  setting = "high"
)

age_vaccination_beta_modifier <- rbind(
  expand.grid(
    dim1 = 1:101,
    dim2 = 2:3,
    dim3 = 1,
    value = subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("1 dose", description)) %>% pull(value)/100
  ),
  expand.grid(
    dim1 = 1:101,
    dim2 = 4:5,
    dim3 = 1,
    value = subset(measles_parameters, parameter == "age_vaccination_beta_modifier" & grepl("2 dose", description)) %>% pull(value)/100
  )
)

#Okay we are going to be using a constant FOI
initial_FOI <- calculate_foi_from_R0(
  R0 = 12,
  contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
  S = model_data_preprocessed$processed_demographic_data$N0[, 4]
)

#Set up model
#Expand time columns
#Add extra vector
extra_time <- 20

tt_seeded = expand_vector(case_vaccination_ready$tt_seeded, extra_time)
tt_birth_changes = expand_vector(model_data_preprocessed$processed_demographic_data$tt_migration, extra_time)
tt_death_changes = expand_vector(model_data_preprocessed$processed_demographic_data$tt_migration, extra_time)
tt_vaccination_coverage = expand_vector(case_vaccination_ready$tt_vaccination, extra_time)
tt_migration = expand_vector(model_data_preprocessed$processed_demographic_data$tt_migration, extra_time)

#Add extra rows
seeded = add_extra_time_rows(df = case_vaccination_ready$seeded, n_extra = extra_time, time_col = "dim4", final_value = 0)
vaccination_coverage = add_extra_time_rows(case_vaccination_ready$vaccination_coverage, n_extra = extra_time, time_col = "dim4", final_value = 0)
crude_birth = add_extra_time_rows(model_data_preprocessed$processed_demographic_data$crude_birth, n_extra = extra_time, time_col = "dim2")
crude_death = add_extra_time_rows(model_data_preprocessed$processed_demographic_data$crude_death, n_extra = extra_time, time_col = "dim3")
migration_in_number = add_extra_time_rows(model_data_preprocessed$processed_demographic_data$migration_in_number, n_extra = extra_time, time_col = "dim4")
migration_distribution_values = add_extra_time_rows(model_data_preprocessed$processed_demographic_data$migration_distribution_values, n_extra = extra_time, time_col = "dim2")

#Loop loads
tiny_params <- sapply(1, function(meow){
  
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
      dim1 = 1:demog_data$input_data$n_age,
      dim2 = 1,
      dim3 = 1,
      value = 0
    ), 
    expand.grid(
      dim1 = 1:demog_data$input_data$n_age,
      dim2 = 2:demog_data$input_data$n_vacc,
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
    short_term_waning = 0,#1/28,
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
    
    tt_seeded = 0:1000,
    seeded = do.call(rbind, sapply(2:1000, function(t) data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = t, value = 1), simplify = FALSE)),#data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 1),
    
    #Demographic parameters
    contact_matrix = demog_data$contact_matrix,
    N0 = demog_data$N0,
    simp_birth_death = 0,
    
    #Birth ages
    repro_low = 15,
    repro_high = 49,
    
  )
  
  
}, simplify = FALSE)[[1]]


#######
new_params <- tiny_params


all_vac_upd <- sapply(seq(0, 100, by = 1), function(x){
  
  vacc_upd <- new_params$vaccination_coverage
  vacc_upd[, 1, , 1] <- x
  vacc_upd
  
}, simplify = FALSE)

new_params$vaccination_coverage <- do.call(abind::abind, c(all_vac_upd, list(along = 4)))
new_params$tt_vaccination_coverage <- 0:(dim(vacc_upd2)[4] - 1)
new_params$no_vacc_changes <- length(tt_vaccination_upd)


#Run model
clean_df <- run_model(
  odin_model = model,
  params = new_params,
  time = 365,
  no_runs = 1
) %>%
  filter(state %in% c("S", "new_case") &
           vaccination != "All")




ggplot(data = clean_df %>% subset(state == "new_case" & age == 5), mapping = aes(x = time, y = value)) + geom_line()

prob_out <- sapply(seq(0, 100, by = 10), function(x){
  
  print(x)
  
  params_upd <- tiny_params
  
  sapply()
  
  params_upd$vaccination_coverage
  
  
  vacc_upd <- abind::abind(params_upd$vaccination_coverage, 
                           params_upd$vaccination_coverage, 
                           params_upd$vaccination_coverage, along = 4)
  vacc_upd[, 1, , 2] <- x
  params_upd$vaccination_coverage <- vacc_upd
  params_upd$tt_vaccination_coverage <- c(0, 1, 2)
  params_upd$no_vacc_changes <- 3
  
  #Run model
  clean_df <- run_model(
    odin_model = model,
    params = params_upd,
    time = 365,
    no_runs = 1000
  ) %>%
    filter(state %in% c("S", "new_case") &
             vaccination != "All")
  
  values <- clean_df %>%
    subset(state == "new_case") %>%
    fgroup_by(time, run) %>%
    fsummarise(value = sum(value)) %>%
    mutate(vac_coverage = x)
  
  cases_total <- clean_df %>%
    subset(state == "new_case") %>%
    group_by(run) %>%
    summarise(value = sum(value))
  
  values %>%
    mutate(above_100 = sum(cases_total$value >= 100)/nrow(cases_total))
  
}, simplify = FALSE)

combo <- Reduce(rbind, prob_out) 

#All together now
combo_sum <- combo %>%
  fgroup_by(vac_coverage, above_100) %>%
  fsummarise(
    mean = mean(value),
    median = median(value),
    low = quantile(value, 0.025),
    high = quantile(value, 0.975)
  )

boop = combo %>%
  fgroup_by(time, state, above_100, vac_coverage) %>%
  fsummarise(
    value = sum(value),
    value_low = sum(value_low),
    value_high = sum(value_high)
  )

#Total case per year
case_per_year <- combo %>%
  subset(state == "new_case") %>%
  fgroup_by(vac_coverage) %>%
  fsummarise(value = sum(value))

case_per_year$time <- 63:54

vac_coverage_plot <- ggplot(
  data = case_per_year,
  mapping = aes(
    x = time + year_start, #100 * vac_coverage,
    y = value
  )
) +
  geom_point() +
  labs(y = "Mean outbreak size",
       x = "") +
  scale_y_continuous(lab = scales::comma, limits = c(0, 15000)) +
  # scale_x_reverse() +
  theme_bw() +
  scale_x_continuous(limits = c(2024, 2033))



protected_plot <- ggplot(data = pop_prot %>%
                           mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))) %>%
                           subset(time > 53 & time < 64),
                         mapping = aes(
                           x = time + year_start,
                           y = prop * 100,
                           fill = status_simple
                         )) +
  geom_bar(width = 1, stat = "identity") +
  theme_bw() +
  labs(
    y = "Percent of the population",
    x = "Year" ,
    fill = ""
  ) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(2024, 2033))

vac_coverage_plot/protected_plot

ggpubr::ggarrange(vac_coverage_plot, protected_plot, ncol = 1, align = "v")




