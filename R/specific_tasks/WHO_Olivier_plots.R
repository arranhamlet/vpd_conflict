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
  ggforce,
  ggpubr,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Run through some countries
countries_of_interest <- c("PSE", "MMR", "SOM", "UKR", "VEN")
  
cases_of_interest <- import("data/processed/WHO/reported_cases_data.csv") %>%
  subset(disease_description == "Measles" & iso3 %in% countries_of_interest) 

country_data <- sapply(countries_of_interest, function(a){

  print(a)

  #Run process
  model_data_processed <- data_load_process_wrapper(
    iso = a,
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
    no_runs = 8
  ), input_data = model_data_processed$input_data)

  export(x = data_clean[[1]],
         file = paste0("output/model_run/WHO_showcase/", a, "_full_data.csv"))

  export(x = data_clean[[2]],
         file = paste0("output/model_run/WHO_showcase/", a, "_susceptibility_data.csv"))

  a

}, simplify = FALSE)


#Load in processed data
all_full_data <- Reduce(rbind, sapply(list.files("output/model_run/WHO_showcase", pattern = "full_data", full.names = T), function(x){
  import(x) %>%
    mutate(iso = first(unlist(strsplit(last(unlist(strsplit(x, "/"))), "_"))))
}, simplify = FALSE))

all_susceptibility_data <- Reduce(rbind, sapply(list.files("output/model_run/WHO_showcase", pattern = "susceptibility_data", full.names = T), function(x){
  import(x) %>%
    mutate(iso = first(unlist(strsplit(last(unlist(strsplit(x, "/"))), "_"))))
}, simplify = FALSE))

#Combine model with case data
all_full_data <- all_full_data %>%
  left_join(cases_of_interest %>%
              select(iso = iso3, year, cases), by = c("iso", "year")) %>%
  mutate(iso_fact = factor(iso))

#Vaccinated
vacc_pop <- all_full_data %>%
  subset(state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All")


#Plot cases over time
all_with_zoom <- sapply(countries_of_interest, function(a){
  
  ggplot(
    data = all_full_data %>%
      subset(age == "All" & state == "new_case" & year >= 1980 & year < 2024 & iso == a),
    mapping = aes(
      x = year,
      y = value,
      ymin = value_min,
      ymax = value_max,
      fill = iso_fact
    )
  ) +
    geom_line() +
    geom_ribbon(alpha = 0.25) +
    theme_bw() + 
    labs(
      x = "",
      y = "Annual cases"
    ) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    facet_zoom(xy = year >= 2000 & year <= 2023, zoom.size = 1, horizontal = F) 
  
}, simplify = FALSE)



case_from_2000 <- ggplot(
  data = all_full_data %>%
    subset(age == "All" & state == "new_case" & year >= 2000 & year < 2024),
  mapping = aes(
    x = year,
    y = value,
    ymin = value_min,
    ymax = value_max
  )
) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  theme_bw() + 
  labs(
    x = "",
    y = "Annual cases"
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  facet_wrap(~iso, scales = "free_y", ncol = 1)

ggpubr::ggarrange(
  case_from_1980, case_from_2000, widths = c(2, 1)
)


#Plot susceptible
suscept_agg <- all_susceptibility_data %>%
  mutate(status_simple = case_when(
    grepl("Vaccine", status) ~ "Vaccinated",
    !grepl("Vaccine", status) ~ "Unvaccinated"
  )) %>%
  group_by(age, year, status_simple, iso) %>%
  summarise(value = sum(value),
            value_min = sum(value_min),
            value_max = sum(value_max),
            .groups = "drop") %>%
  group_by(age, year, iso) %>%
  mutate(prop = value/sum(value),
         prop_min = value_min/sum(value_min),
         prop_max = value_max/sum(value_max))

ggplot(
  data = suscept_agg %>%
    subset(status_simple == "Vaccinated" & year >= 1980 & age %in% c(2, 18, 30)),
  mapping = aes(
    x = year,
    y = prop * 100,
    fill = as.factor(iso),
    color = as.factor(iso),
    ymin = prop_min * 100,
    ymax = prop_max * 100
  )
) +
  geom_line() +
  geom_ribbon(alpha = 0.25) + 
  theme_bw() +
  labs(x = "",
       y = "% vaccinated",
       color = "",
       fill = "") +
  facet_wrap(~age)

ggplot(data = suscept_agg %>%
         subset(year == 2023),
       mapping = aes(
         x = as.numeric(age),
         y = value,
         fill = status
       )) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  labs(x = "Age",
       y = "Number",
       fill = "") +
  scale_y_continuous(
    labels = scales::comma
  ) +
  facet_wrap(~iso, scales = "free_y")



