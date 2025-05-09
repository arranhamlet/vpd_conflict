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
vaccination_schedule <- import("data/processed/WHO/vaccine-schedule-data.xlsx")
measles_parameters <- import(here("data", "processed", "model_parameters", "Measles_SEIR_Parameters.csv"))
sia_vaccination <- import("data/processed/vaccination/sia_vimc.rds")
VIMC_vaccination <- import("C:/Users/ah1114/Documents/Imperial/VPD_conflict/generic_vpd_models/data/processed/vaccination/coverage_table.rds")

#Run processing
model_data_preprocessed <- model_input_formatter_wrapper(
  iso = "GBR",    
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
  year_start = 1950
)

#Take pre-processed case and vaccination data and get it ready for params
case_vaccination_ready <- case_vaccine_to_param(
  demog_data = model_data_preprocessed$processed_demographic_data,
  processed_vaccination = model_data_preprocessed$processed_vaccination_data,
  processed_vaccination_sia = model_data_preprocessed$processed_vaccination_sia,
  processed_case = model_data_preprocessed$processed_case_data,
  vaccination_schedule = vaccination_schedule
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
  R0 = 18,
  contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
  N = model_data_preprocessed$processed_demographic_data$N0[, 4],
  infectious_period = 365/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value)
)

#Set up model
params <- param_packager(
  
  # Demographic parameters
  n_age = model_data_preprocessed$processed_demographic_data$input_data$n_age,
  n_vacc = model_data_preprocessed$processed_demographic_data$input_data$n_vacc,
  n_risk = model_data_preprocessed$processed_demographic_data$input_data$n_risk,
  
  # Vaccine parameters
  short_term_waning = 1/(14/365),
  long_term_waning = 0,#1/subset(measles_parameters, parameter == "long_term_waning" & grepl("2 dose", description)) %>% pull(value),
  age_vaccination_beta_modifier = age_vaccination_beta_modifier,
  
  # Disease parameters 
  R0 = 18,
  tt_R0 = 0,
  user_specified_foi = 1,
  initial_FOI = initial_FOI,

  #Disease parameters
  cfr_normal = 0,
  cfr_severe = (subset(measles_parameters, parameter == "cfr") %>% pull(value) %>% median())/100,
  incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value) * 365,
  recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  severe_recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  
  #Seeding previous cases
  I0 = data.frame(dim1 = 18, dim2 = 1, dim3 = 1, dim4 = 1, value = 1),
  # tt_seeded = case_vaccination_ready$tt_seeded,
  # seeded = case_vaccination_ready$seeded,
  #Setting up vaccination
  vaccination_coverage = case_vaccination_ready$vaccination_coverage,
  tt_vaccination_coverage = case_vaccination_ready$tt_vaccination, 

  #Demographic parameters
  contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
  N0 = model_data_preprocessed$processed_demographic_data$N0,
  #List of when birth_death_changes
  tt_birth_changes = model_data_preprocessed$processed_demographic_data$tt_migration,
  tt_death_changes = model_data_preprocessed$processed_demographic_data$tt_migration,
  crude_birth = model_data_preprocessed$processed_demographic_data$crude_birth,
  crude_death = model_data_preprocessed$processed_demographic_data$crude_death,
  simp_birth_death = 0,
  aging_rate = 1,
  tt_migration = model_data_preprocessed$processed_demographic_data$tt_migration,
  # migration_in_number = model_data_preprocessed$processed_demographic_data$migration_in_number,
  # migration_distribution_values = model_data_preprocessed$processed_demographic_data$migration_distribution_values,

  #Birth ages
  repro_low = 15,
  repro_high = 49,
  age_maternal_protection_ends = 1,
  protection_weight_vacc = 1,
  protection_weight_rec = 1,
  migration_represent_current_pop = 1

)

#Run model
clean_df <- run_model(
  odin_model = model,
  params = params,
  time = (model_data_preprocessed$processed_demographic_data$input_data$year_end - model_data_preprocessed$processed_demographic_data$input_data$year_start) + 1,
  no_runs = 1
)

#Plot total population
year_start <- model_data_preprocessed$processed_demographic_data$input_data$year_start

#Plot
ggplot(
  data = clean_df %>%
    filter(state %in% c("S", "E", "I", "R", "Is", "Rc") & age == "All"),
  mapping = aes(
    x = time + year_start,
    y = value
  )
) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Population"
  ) +
  scale_y_continuous(label = scales::comma) +
  theme_bw() +
  facet_wrap(~state, scales = "free_y")

#Plot
ggplot(
  data = clean_df %>%
    filter(state %in% c("new_case") & age == "All" & time > 5),
  mapping = aes(
    x = time + year_start,
    y = value
  )
) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Cases"
  ) +
  scale_y_continuous(label = scales::comma) +
  theme_bw() +
  facet_wrap(~state, scales = "free_y")

# ggplot(
#   data = clean_df %>%
#     filter(state %in% c("FOI_scale_sum") & age != "All" & time > 50),
#   mapping = aes(
#     x = time + year_start,
#     y = value,
#     color = age
#   )
# ) +
#   geom_line(stat = "identity") +
#   labs(
#     x = "Year",
#     y = "FOI scale"
#   ) +
#   scale_y_continuous(label = scales::comma) +
#   theme_bw() +
#   facet_wrap(~as.numeric(age))


ggplot(
  data = clean_df %>%
    filter(state %in% c("Reff") & age == "All" & time > 5),
  mapping = aes(
    x = time + year_start,
    y = value
  )
) +
  geom_line(stat = "identity") +
  labs(
    x = "Year",
    y = "Reff"
  ) +
  scale_y_continuous(label = scales::comma) +
  theme_bw() +
  facet_wrap(~state, scales = "free_y") +
  geom_hline(yintercept = 1)



#Plot
vacc_agg <- clean_df %>%
  filter(vaccination != "All" & state %in% c("S", "E", "I", "R", "Is", "Rc")) %>%
  group_by(time, vaccination) %>%
  summarise(
    value = sum(value),
    .groups = "drop"
  ) %>%
  group_by(
    time
  ) %>%
  mutate(coverage = value/sum(value, na.rm = T))

ggplot(
  data = vacc_agg,
  mapping = aes(
    x = time,
    y = coverage,
    color = vaccination
  )
) +
  geom_line() +
  theme_bw()


#For select ages
vacc_age <- subset(clean_df, state %in% c("S", "E", "I", "R", "Is", "Rc")) %>%
  ungroup() %>%
  fgroup_by(time, age, vaccination) %>%
  fsummarise(value = sum(value)) %>%
  group_by(time, age, vaccination) %>%
  mutate(
    coverage = value/sum(value, na.rm = T),
    coverage = case_when(
      is.nan(coverage) ~ 0,
      !is.nan(coverage) ~ coverage
    )
  )

vaccine_by_age <- ggplot(
  data = vacc_age %>%
    subset(time == max(time) &
             age != "All") %>%
    mutate(vaccination = case_when(
      vaccination == 1 ~ "Unvaccinated",
      vaccination %in% 2:3 ~ "1 dose",
      vaccination %in% 4:5 ~ "2 doses",
    ),
    vaccination = factor(vaccination, levels = c("Unvaccinated", "1 dose", "2 doses"))),
  mapping = aes(
    x = as.numeric(age),
    y = value,
    fill = vaccination
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    x = "Age",
    y = "Population",
    fill = "",
    title = "Measles vaccination coverage (2024)"
  ) +
  scale_y_continuous(labels = scales::comma)


#Okay more complicated plot, combine vaccination and R to create graph of those immune
susceptibility_data <- subset(clean_df, state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All") %>%
  mutate(
    status = case_when(
      state == "S" & vaccination == 1 ~ "Susceptible",
      state == "S" & vaccination > 1 ~ "Vaccine protected",
      state != "S" & vaccination == 1 ~ "Exposure protected",
      state != "S" & vaccination > 1 ~ "Vaccine and exposure protected"
      
    ),
    status = factor(status, levels = c("Susceptible", "Vaccine protected", "Exposure protected", 
                                       "Vaccine and exposure protected"))
  ) %>%
  group_by(
    time, age, status
  ) %>%
  summarise(
    value = sum(value),
    .groups = "keep"
  ) %>%
  mutate(
    coverage = value/sum(value, na.rm = T),
    coverage = case_when(
      is.nan(coverage) ~ 0,
      !is.nan(coverage) ~ coverage
    )
  )

protection_by_age <- ggplot(
  data = susceptibility_data %>%
    subset(time == max(time)),
  mapping = aes(
    x = as.numeric(age),
    y = value,
    fill = status
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    x = "Age",
    y = "Population",
    fill = "",
    title = "Measles susceptibility (2024)"
  ) +
  scale_y_continuous(labels = scales::comma)

vaccine_by_age / protection_by_age


#Overall protection in the population
pop_prot <- susceptibility_data %>%
  mutate(
    status_simple = case_when(
      status == "Susceptible" ~ "Susceptible",
      status != "Susceptible" ~ "Protected"
    )
  ) %>%
  group_by(time, status_simple) %>%
  summarise(
    value = sum(value)
  ) %>%
  group_by(time) %>%
  mutate(
    prop = value/sum(value)
  )

pop_prot_age <- susceptibility_data %>%
  mutate(
    status_simple = case_when(
      status == "Susceptible" ~ "Susceptible",
      status != "Susceptible" ~ "Protected"
    )
  ) %>%
  fgroup_by(time, age, status_simple) %>%
  fsummarise(
    value = sum(value)
  ) %>%
  group_by(time, age) %>%
  mutate(
    prop = value/sum(value)
  )



ggplot(data = pop_prot %>%
         mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))) %>%
         subset(time >= 1),
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
  )

ggplot(data = pop_prot %>%
         mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))) %>%
         subset(status_simple == "Protected" & time > 5),
       mapping = aes(
         x = time + year_start,
         y = prop * 100,
         fill = status_simple
       )) +
  geom_line() +
  theme_bw() +
  labs(
    y = "Population level vaccination coverage (%)",
    x = "Year" ,
    fill = ""
  )

#UKHSA susceptibility
susceptibility <- import("data/processed/vaccination/UKHSA_susceptability_measles.xlsx") %>%
  clean_names() %>%
  group_by(birth_year) %>%
  mutate(
    birth_year = as.numeric(unlist(strsplit(birth_year[1], " "))[1]),
    age = 2023 - birth_year,
    percent_susceptible = percent_susceptible/100
  ) %>%
  select(birth_year, age, percent_susceptible)

ukhsa_compare <- subset(pop_prot_age, time == max(time) & status_simple == "Susceptible" & as.numeric(age) >= 7) %>% 
  arrange(as.numeric(age)) %>% 
  mutate(birth_year = 2023 - as.numeric(age),
         prop_adjusted = median(c(0, prop))) %>% 
  select(birth_year, time, age, prop, prop_adjusted)

ukhsa_model_comp <- ukhsa_compare %>%
  left_join(susceptibility, "birth_year") %>%
  subset(!is.na(age.y)) %>%
  select(birth_year, model_prop = prop, model_prop_adjusted = prop_adjusted, UKHSA_adjusted = percent_susceptible) %>%
  gather(key = "measurement", value = "susceptible", - c(birth_year, time))
  
#Plot this madness
ggplot(
  data = ukhsa_model_comp %>%
    subset(measurement != "model_prop_adjusted"),
  mapping = aes(
    x = birth_year,
    y = susceptible * 100,
    color = measurement
  )
) +
  geom_line() +
  theme_bw() +
  labs(x = "Birth year",
       y = "Susceptible (%)",
       color = "")

