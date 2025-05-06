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

params <- param_packager(
  
  # Demographic parameters
  n_age = model_data_preprocessed$processed_demographic_data$input_data$n_age,
  n_vacc = model_data_preprocessed$processed_demographic_data$input_data$n_vacc,
  n_risk = model_data_preprocessed$processed_demographic_data$input_data$n_risk,
  
  # Vaccine parameters
  short_term_waning = 1/(14/365),
  age_vaccination_beta_modifier = age_vaccination_beta_modifier,
  
  # Disease parameters 
  R0 = 0,
  initial_FOI = 0,
  
  #Disease parameters
  incubation_rate = 1/subset(measles_parameters, parameter == "incubation_period") %>% pull(value) * 365,
  recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  severe_recovery_rate = 1/subset(measles_parameters, parameter == "recovery_rate") %>% pull(value) * 365,
  
  #Seeding previous cases
  I0 = 0,
  
  tt_seeded = tt_seeded,
  seeded = seeded,
  vaccination_coverage = vaccination_coverage,
  tt_vaccination_coverage = tt_vaccination_coverage,
  tt_birth_changes = tt_birth_changes,
  tt_death_changes = tt_death_changes,
  crude_birth = crude_birth,
  crude_death = crude_death,
  tt_migration = tt_migration,
  migration_in_number = migration_in_number,
  migration_distribution_values = migration_distribution_values,
  
  #Demographic parameters
  contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
  N0 = model_data_preprocessed$processed_demographic_data$N0,
  #List of when birth_death_changes
  
  simp_birth_death = 0,
  aging_rate = 1,
  
  
  #Birth ages
  repro_low = 15,
  repro_high = 49,
  age_maternal_protection_ends = 1,
  protection_weight_vacc = .5,
  protection_weight_rec = .5,
  
)

#Run model
clean_df <- run_model(
  odin_model = model,
  params = params,
  time = (model_data_preprocessed$processed_demographic_data$input_data$year_end - model_data_preprocessed$processed_demographic_data$input_data$year_start) + extra_time,
  no_runs = 1
)

#Plot total population
year_start <- model_data_preprocessed$processed_demographic_data$input_data$year_start

#Plot
ggplot(
  data = clean_df %>%
    filter(state == "I" & age == "All"),
  mapping = aes(
    x = time + year_start,
    y = value
  )
) +
  geom_line() +
  labs(
    x = "Year",
    y = "Cases"
  ) +
  scale_y_continuous(label = scales::comma) +
  theme_bw()



#Plot
ggplot(
  data = clean_df %>%
    filter(state %in% c("S", "E", "I", "R", "Is", "Rc") & age == "All" & time > 5),
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


#For select ages
vacc_age <- subset(clean_df, state %in% c("S", "E", "I", "R", "Is", "Rc")) %>%
  ungroup() %>%
  group_by(time, age, vaccination) %>%
  summarise(value = sum(value)) %>%
  mutate(
    coverage = value/sum(value, na.rm = T),
    coverage = case_when(
      is.nan(coverage) ~ 0,
      !is.nan(coverage) ~ coverage
    )
  )

#Okay more complicated plot, combine vaccination and R to create graph of those immune
susceptibility_data <- subset(clean_df, state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All") %>%
  mutate(
    status = case_when(
      state == "S" & vaccination == 1 ~ "Susceptible",
      state == "S" & vaccination > 1 ~ "Vaccine protected",
      state %in% c("S", "E", "I", "R", "Is", "Rc") & vaccination == 1 ~ "Exposure protected",
      state %in% c("S", "E", "I", "R", "Is", "Rc") & vaccination > 1 ~ "Vaccine and exposure protected"
      
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


ggplot(data = pop_prot %>%
         mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))),
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

#
ggplot(data = pop_prot %>%
         mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))) %>%
         subset(time >= 50 & status_simple == "Protected"),
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

#Loop through different vaccination thresholds
vaccine_value <- pop_prot %>%
  mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected"))) %>%
  subset(time >= 50 & status_simple == "Protected")

vacc_use <- vaccine_value %>% subset(time >= 54 & time < 64)


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
    
    tt_seeded = c(0, 2, 3),
    seeded = data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 1),
    
    #Demographic parameters
    contact_matrix = demog_data$contact_matrix,
    N0 = demog_data$N0 %>%
      mutate(value = value * 1),
    simp_birth_death = 0,
    
    #Birth ages
    repro_low = 15,
    repro_high = 49,
    
  )
  
  
}, simplify = FALSE)[[1]]


prob_out <- sapply(seq(0.98, 0.82, by = -0.02), function(x){
  
  print(x)
  
  params_upd <- tiny_params
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
    time = 365 * 3,
    no_runs = 25
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

#Annual cases
combo_annual <- combo %>%
  fgroup_by(vac_coverage, above_100, run) %>%
  fsummarise(
    value = sum(value)
  )

get_95CI <- function(x, method = c("normal", "t.test", "bootstrap"), conf_level = 0.95, n_boot = 1000, type) {
  method <- match.arg(method)
  
  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  x <- x[!is.na(x)]
  if (length(x) < 2) stop("Need at least two non-missing values.")
  
  alpha <- 1 - conf_level
  mean_x <- mean(x)
  
  if (method == "normal") {
    se_x <- sd(x) / sqrt(length(x))
    z <- qnorm(1 - alpha / 2)
    ci <- c(mean_x - z * se_x, mean_x + z * se_x)
    
  } else if (method == "t.test") {
    ci <- t.test(x, conf.level = conf_level)$conf.int
    
  } else if (method == "bootstrap") {
    boot_means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
    ci <- quantile(boot_means, probs = c(alpha / 2, 1 - alpha / 2))
  }
  
  if(type == "mean"){
    mean_x
  } else if(type == "low"){
    ci[1]
  } else if(type == "high"){
    ci[2]
  }
}



#All together now
combo_sum <- combo_annual %>%
  fgroup_by(vac_coverage, above_100) %>%
  fsummarise(
    mean = get_95CI(value, method = "bootstrap", type = "mean"),
    median = median(value),
    low = get_95CI(value, method = "bootstrap", type = "low"),
    high = get_95CI(value, method = "bootstrap", type = "high")
  )
# boop = combo %>%
#   fgroup_by(time, state, above_100, vac_coverage) %>%
#   fsummarise(
#     value = sum(value),
#     value_low = sum(value_low),
#     value_high = sum(value_high)
#   )

#Total case per year
# case_per_year <- combo %>%
#   # subset(state == "new_case") %>%
#   fgroup_by(vac_coverage) %>%
#   fsummarise(value = sum(value))

combo_sum$time <- 63:55

vac_coverage_plot <- ggplot(
  data = combo_sum,
  mapping = aes(
    x = time + year_start, #100 * vac_coverage,
    ymin = low,
    ymax = high,
    y = mean
  )
) +
  geom_point() +
  geom_errorbar() +
  labs(y = "Mean outbreak size",
       x = "") +
  scale_y_continuous(lab = scales::comma) +
  # scale_x_reverse() +
  theme_bw() +
  coord_cartesian(xlim = c(2025, 2033)) +
  theme(axis.title.x = element_blank(), 
        axis.line.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(plot.margin = unit(rep(0, 4), "pt"))  +
  geom_hline(yintercept = subset(combo_sum, vac_coverage  == max(vac_coverage )) %>% pull(mean), linetype = "dashed")


prot_df <- data.frame(
  time = 63:55,
  status_simple = "Protected",
  prop = combo_sum$vac_coverage
) %>%
  rbind(
    data.frame(
      time = 63:55,
      status_simple = "Susceptible",
      prop = 1 - combo_sum$vac_coverage
    )
  )%>%
  mutate(status_simple = factor(status_simple, levels = c("Susceptible", "Protected")))


protected_plot <- ggplot(data = prot_df ,
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
  coord_cartesian(xlim = c(2025, 2033), ylim = c(80, 100)) +
  theme(plot.margin = unit(rep(0, 4), "pt")) +
  geom_hline(yintercept = 100 * max(combo_sum$vac_coverage), linetype = "dashed") +
  scale_fill_manual(values = c("red", "grey80"))

outbreak_cases <- ggplot(
  data = combo %>%
    mutate(
      facet_title = paste0("Population immunity: ", vac_coverage * 100, "%"),
      facet_title = factor(facet_title,
                           levels = paste0("Population immunity: ", unique(combo$vac_coverage) * 100, "%"))
    ),
  mapping = aes(
    x = time,
    y = value,
    group = run,
  )
) +
  geom_line(color = "gray90") +
  facet_wrap(~facet_title) +
  theme_bw() +
  # coord_cartesian(xlim = c(0, 365)) +
  scale_y_continuous(lab = scales::comma) +
  geom_text(mapping = aes(x = 675, y = 10000, label = paste0(100 * above_100, "% of introductions led to an\noutbreak of more than 100 cases")), size = 2) +
  labs(x = "Days",
       y = "Cases")
  


ontop_plot <- (vac_coverage_plot / protected_plot)
# 
# giga_plot <- ontop_plot | outbreak_cases + plot_annotation(
#   title = "Relationship of population level vaccination coverage and outbreak size for measles.", 
#   subtitle = "Disruption of routine vaccination in 2024 increases the susceptible population to over 30%, driving a 4x increase in mean outbreak size—highlighting critical humanitarian response needs.",
#   tag_levels  = "A")
# 
# ggsave(plot = giga_plot, filename = "figs/population_protection_outbreak_size.jpg", height = 5, width = 12)




# ggpubr_alt <- ggpubr::ggarrange(ggpubr::ggarrange(vac_coverage_plot, protected_plot, ncol = 1, vjust = 1), 
#                                 outbreak_cases, ncol = 2, widths = c(1, 2))

ggsave(plot = ggpubr::ggarrange(ontop_plot, outbreak_cases, ncol = 2, widths = c(1, 1.5)), filename = "figs/population_protection_outbreak_size.jpg", height = 6, width = 10)


ggpubr::ggarrange(ontop_plot, outbreak_cases, ncol = 2, widths = c(1, 1.5)) + plot_annotation(
      title = "Relationship of population level vaccination coverage and outbreak size for measles.",
      subtitle = "Disruption of routine vaccination in 2025 ",
      tag_levels  = "A")
