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
model_data_processed <- data_load_process_wrapper(
  iso = "GBR",
  disease = "measles",
  vaccine = "measles",
  R0 = 12,
  timestep = "day"
)

#Process for plotting
data_clean <- process_for_plotting(run_model_output = run_model(
  odin_model = model,
  params = model_data_processed$params,
  time = floor(model_data_processed$time),
  no_runs = 2
), input_data = model_data_processed$input_data)






# 
# ggplot(
#   data = subset(clean$aggregate_df, state == "new_case" & age == "All" & year >= 1960),
#   mapping = aes(
#     x = year,
#     y = value,
#     ymin = value_min,
#     ymax = value_max
#   )
# ) +
#   geom_line() +
#   geom_ribbon()
# 
# 
susc_agg <- data_clean$susceptibility_data %>%
  group_by(age, year, status) %>%
  summarise(
    value = sum(value),
    value_max = sum(value_max),
    value_min = sum(value_min),
    .groups = "drop"
  ) %>%
  group_by(age, year) %>%
  mutate(prop = value/sum(value)) %>%
  mutate(status_vacc = case_when(
    grepl("vaccine", status, ignore.case = T) ~ "Vaccine protected",
    !grepl("vaccine", status, ignore.case = T) ~ "Susceptible"
  ))
# 
# 
# ggplot(
#   data = subset(susc_agg, year == 2023),
#   mapping = aes(
#     x = as.numeric(age),
#     y = value,
#     fill = status
#   )
# ) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   labs(x = "", y = "", fill = "")

# 
ggplot(
  data = subset(susc_agg, year >= 1990 & age == 3 & status == "Vaccine protected"),
  mapping = aes(
    x = as.numeric(year),
    y = prop,
    fill = status_vacc
  )
) +
  geom_line(stat = "identity") +
  theme_bw() +
  labs(x = "", y = "", fill = "") 

