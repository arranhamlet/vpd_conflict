options(scipen = 999)

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

#Run process
GBR_day <- data_load_process_wrapper(
    iso = "GBR",
    disease = "measles",
    vaccine = "measles",
    R0 = 18,
    timestep = "day"
)

# GBR_month <- data_load_process_wrapper(
#   iso = "GBR",
#   disease = "measles",
#   vaccine = "measles",
#   R0 = 18,
#   timestep = "month"
# )

GBR_year <- data_load_process_wrapper(
  iso = "GBR",
  disease = "measles",
  vaccine = "measles",
  R0 = 18,
  timestep = "year"
)

#Process for plotting
GBR_day_clean <- process_for_plotting(run_model_output = run_model(
  odin_model = model,
  params = GBR_day$params,
  time = floor(GBR_day$time),
  no_runs = 2
), input_data = GBR_day$input_data)

# GBR_month_clean <- process_for_plotting(run_model_output = run_model(
#   odin_model = model,
#   params = GBR_month$params,
#   time = floor(GBR_month$time),
#   no_runs = 2
# ), input_data = GBR_month$input_data)

GBR_year_clean <- process_for_plotting(run_model_output = run_model(
  odin_model = model,
  params = GBR_year$params,
  time = floor(GBR_year$time),
  no_runs = 2
), input_data = GBR_year$input_data)

#Combine
total_data <- rbind(
  GBR_day_clean$aggregate_df %>%
    mutate(time_adjust = GBR_day$input_data$time_adjust),
  # GBR_month_clean$aggregate_df %>%
  #   mutate(time_adjust = GBR_month$input_data$time_adjust),
  GBR_year_clean$aggregate_df %>%
    mutate(time_adjust = GBR_year$input_data$time_adjust)
)

total_data_age <- total_data %>%
  subset(year == 2023 & age != "All" & state %in% c("S", "E", "I", "R", "Is", "Rc")) %>%
  fgroup_by(age, year, time_adjust) %>%
  fsummarise(value = sum(value),
             value_min = sum(value_min),
             value_max = sum(value_max)) %>%
  group_by(year, time_adjust) %>%
  mutate(prop_value = value/sum(value),
         prop_value_min = value_min/sum(value_min),
         prop_value_max = value_max/sum(value_max))

ggplot(data = total_data_age %>%
         subset(age != "All"), 
       mapping = aes(
         x = as.numeric(age), 
         y = value, 
         ymin = value_min, 
         ymax = value_max, 
         color = as.factor(time_adjust), 
         fill = as.factor(time_adjust))
       ) + 
  geom_line() +
  geom_ribbon(alpha = 0.25) +
  labs(x = "Age",
       y = "Number",
       fill = "Timestep",
       color = "Timestep") +
  theme_bw() +
  scale_y_continuous()

#Age in
ggplot(data = total_data_age %>%
         subset(age != "All"), 
       mapping = aes(
         x = as.numeric(age), 
         y = prop_value * 100, 
         ymin = prop_value_min * 100,
         ymax = prop_value_max * 100,
         fill = as.factor(time_adjust),
         color = as.factor(time_adjust))
) + 
  geom_line() +
  geom_ribbon(alpha = 0.25) + 
  labs(x = "Age",
       y = "Percent of population",
       fill = "Timestep",
       color = "Timestep",
       title = "Proportion of population in each age for different timesteps (GBR 2023)",
       subtitle = "Averaged across 4 model runs") +
  theme_bw() +
  scale_y_continuous()

ggsave("figs/demography/GBR_timestep.jpg", height = 4, width = 7)

ggplot(data = total_data %>% 
         subset(state == "total_pop" & year <= 2023) %>%
         mutate(time_adjust = case_when(
           time_adjust == 1 ~ "Day",
           time_adjust == 7 ~ "Week",
           time_adjust == 30 ~ "Month",
           time_adjust == 365 ~ "Year"
         )), 
       mapping = aes(x = year, y = value, color = as.factor(time_adjust),
                     ymin = value_min, ymax = value_max, fill = as.factor(time_adjust))) + 
  geom_line() +
  geom_ribbon(alpha = 0.1) +
  labs(x = "",
       y = "Total population",
       fill = "Timestep",
       color = "Timestep",
       title = "Total population over years estimate (GBR)",
       subtitle = "Averaged across 4 model runs") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

ggsave("figs/demography/GBR_total_population_timestep.jpg", height = 4, width = 7)


whole_time <- ggplot(data = total_data %>% 
         subset(year > 1950 & state == "new_case" & age == "All" & year <= 2023) %>%
         mutate(time_adjust = case_when(
           time_adjust == 1 ~ "Day",
           time_adjust == 7 ~ "Week",
           time_adjust == 30 ~ "Month",
           time_adjust == 365 ~ "Year"
         )), 
       mapping = aes(x = year, y = value, fill = as.factor(time_adjust), ymin = value_min, ymax = value_max)) + 
  geom_line() +
  geom_ribbon(alpha = 0.25) + 
  facet_wrap(~time_adjust, scales = "free_y", ncol = 1) +
   theme_bw()  +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "",
       y = "Annual cases",
       fill = "Timestep",
       color = "Timestep")

last_30_years <- ggplot(data = total_data %>% 
                       subset(year > 1995 & state == "new_case" & age == "All" & year <= 2023) %>%
                       mutate(time_adjust = case_when(
                         time_adjust == 1 ~ "Day",
                         time_adjust == 7 ~ "Week",
                         time_adjust == 30 ~ "Month",
                         time_adjust == 365 ~ "Year"
                       )), 
                     mapping = aes(x = year, y = value, fill = as.factor(time_adjust), ymin = value_min, ymax = value_max)) + 
  geom_line() +
  geom_ribbon(alpha = 0.25) + 
  facet_wrap(~time_adjust, scales = "free_y", ncol = 1) +
  theme_bw()  +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "",
       y = "",
       fill = "Timestep",
       color = "Timestep")

combo_cases <- ggpubr::ggarrange(whole_time, last_30_years, common.legend = T, legend = "bottom", widths = c(2, 1))

ggsave("figs/demography/GBR_cases_over_time_timestep.jpg", height = 5, width = 10)


total_susceptible <- total_data %>%
  subset(state %in% c("S", "E", "I", "R", "Is", "Rc")) %>%
  mutate(status = case_when(
    state == "S" & vaccination == 1 ~ "Susceptible",
    state == "S" & vaccination > 1 ~ "Vaccine protected",
    state != "S" & vaccination == 1 ~ "Exposure protected",
    state != "S" & vaccination > 1 ~ "Vaccine and exposure protected"
  )) %>%
  fgroup_by(age, year, time_adjust, status) %>%
  fsummarise(
    value = sum(value),
    value_min = sum(value_min),
    value_max = sum(value_max)
  ) %>%
  group_by(
    age, year, time_adjust
  ) %>%
  mutate(prop = value/sum(value),
         prop_min = value_min/sum(value_min),
         prop_max = value_max/sum(value_max)) %>%
  mutate(
    status = factor(status, levels = c("Susceptible", "Vaccine protected", "Exposure protected", 
                              "Vaccine and exposure protected"))
  )
  

ggplot(
  data = total_susceptible %>%
    subset(year > 2000 & age == 2 & status == "Susceptible"),
  mapping = aes(
    x = as.numeric(year),
    y = prop,
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
  facet_wrap(~time_adjust) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma)


  
total_susceptible <- rbind(
  GBR_day_clean$susceptibility_data %>%
    mutate(time_adjust = GBR_day$input_data$time_adjust),
  # GBR_month_clean$susceptibility_data %>%
  #   mutate(time_adjust = GBR_month$input_data$time_adjust),
  GBR_year_clean$susceptibility_data %>%
    mutate(time_adjust = GBR_year$input_data$time_adjust)
)


protection_by_age <- ggplot(
  data = total_susceptible %>%
    subset(year == 2023),
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
  facet_wrap(~time_adjust) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma)

ggsave("figs/demography/GBR_protection_by_age_time_timestep.jpg", height = 5, width = 10)


