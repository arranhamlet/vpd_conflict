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

GBR_year <- data_load_process_wrapper(
  iso = "GBR",
  disease = "measles",
  vaccine = "measles",
  R0 = 18,
  timestep = "month"
)

#Run model
GBR_day_processed <- run_model(
  odin_model = model,
  params = GBR_day$params,
  time = floor(GBR_day$time),
  no_runs = 1
)

GBR_year_processed <- run_model(
  odin_model = model,
  params = GBR_year$params,
  time = floor(GBR_year$time),
  no_runs = 1
)

GBR_day_clean <- process_for_plotting(run_model_output = GBR_day_processed, input_data = GBR_day$input_data)
GBR_year_clean <- process_for_plotting(run_model_output = GBR_year_processed, input_data = GBR_year$input_data)

total_data <- rbind(
  GBR_day_clean$aggregate_df %>%
    mutate(time_adjust = GBR_day$input_data$time_adjust),
  GBR_year_clean$aggregate_df %>%
    mutate(time_adjust = GBR_year$input_data$time_adjust)
)

ggplot(data = total_data %>% 
         subset(state == "total_pop"), 
       mapping = aes(x = year, y = value, fill = time_adjust)) + 
  geom_bar(stat = "identity", position = position_dodge())


ggplot(data = total_data %>% 
         subset(state == "I" & age == "All"), 
       mapping = aes(x = year, y = value, fill = as.factor(time_adjust))) + 
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  facet_wrap(~time_adjust, scales = "free_y")


total_susceptible <- rbind(
  GBR_day_clean$susceptibility_data %>%
    mutate(time_adjust = GBR_day$input_data$time_adjust),
  GBR_year_clean$susceptibility_data %>%
    mutate(time_adjust = GBR_year$input_data$time_adjust)
)



protection_by_age <- ggplot(
  data = total_susceptible %>%
    subset(year == max(year)),
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
  facet_wrap(~time_adjust)



