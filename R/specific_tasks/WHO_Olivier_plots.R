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
countries_of_interest <- c("PSE", "MMR", "SOM", "UKR", "VEN", "GBR")
  
cases_of_interest <- import("data/processed/WHO/reported_cases_data.csv") %>%
  subset(disease_description == "Measles" & iso3 %in% countries_of_interest) 

population_all <- import(here("data", "processed", "WPP", "age_both.csv")) %>%
  subset(iso3 %in% countries_of_interest)

countries_of_interest <- "FRA"
  
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
    no_runs = 2
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
  
  plot <- ggplot(
  ) +
    geom_line(data = all_full_data %>%
                subset(age == "All" & state == "new_case" & year >= 1980 & year < 2024 & iso == a),
              mapping = aes(
                x = year,
                y = value
              )) +
    geom_ribbon(data = all_full_data %>%
                  subset(age == "All" & state == "new_case" & year >= 1980 & year < 2024 & iso == a),
                mapping = aes(
                  x = year,
                  y = value,
                  ymin = value_min,
                  ymax = value_max
                ),
                alpha = 0.25) +
    theme_bw() + 
    labs(
      x = "",
      y = "Annual cases",
      title = a
    ) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    facet_zoom(xy = year >= 2000 & year <= 2023, zoom.size = 1, horizontal = F) +
    geom_bar(
      data = cases_of_interest %>%
        subset(iso3 == a),
      mapping = aes(
        x = year,
        y = cases
      ),
      stat = "identity",
      fill = "red"
    )
  
  ggsave(plot = plot, filename = paste0("figs/presentations/20251205/", a, "_cases_of_measles.jpg"), width = 6, height = 4)
  
}, simplify = FALSE)



ggplot(
  data = all_full_data %>%
    subset(age == "All" & state == "new_case" & year >= 1980 & year < 2024 & iso == "MMR"),
  mapping = aes(
    x = year,
    y = value,
    ymin = value_min,
    ymax = value_max
  )
) +
  geom_line() +
  geom_ribbon(alpha = 0.25) +
  theme_bw() + 
  labs(
    x = "",
    y = "Annual cases",
    title = a
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  facet_zoom(xy = year >= 2000 & year <= 2023, zoom.size = 1, horizontal = F) 






#Plot susceptible
suscept_agg <- all_susceptibility_data %>%
  mutate(status_simple = case_when(
    grepl("Vaccine", status) ~ "Vaccine protected",
    !grepl("Vaccine", status) ~ "Susceptible"
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

suscept_complex_agg <- all_susceptibility_data %>%
  group_by(age, year, status, iso) %>%
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
    subset(iso == "FRA" & status_simple == "Vaccine protected" & year >= 1980 & age %in% c(2, 18, 30)) %>%
    mutate(age = as.numeric(age),
           age_text = paste0(age, " years old"),
           age_text = factor(age_text, levels = c("2 years old",
                                                  "18 years old",
                                                  "30 years old"))),
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
       title = "Vaccination coverage at select ages 1980-2023",
       color = "",
       fill = "") +
  facet_wrap(~age_text)

ggsave("figs/presentations/20251205/MMR_immunization_coverage_time.jpg", width = 8, height = 4)



ggplot(
  data = suscept_agg %>%
    subset(status_simple == "Vaccine protected" & year >= 1980 & age %in% c(2, 18, 30)) %>%
    mutate(age = as.numeric(age),
           age_text = paste0(age, " years old"),
           age_text = factor(age_text, levels = c("2 years old",
                                                  "18 years old",
                                                  "30 years old"))),
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
       title = "Vaccination coverage at select ages 1980-2023",
       color = "",
       fill = "") +
  facet_wrap(~age_text)

ggsave("figs/presentations/20251205/immunization_coverage_time.jpg", width = 8, height = 4)

cols <- c("Susceptible" = "#F8766D", "Vaccine protected" = "#00BFC4", "Exposure protected" = "#7CAE00", "Vaccine and exposure protected" = "#C77CFF")


ggplot(data = suscept_agg %>%
         subset(year == 2023 & iso == "MMR"),
       mapping = aes(
         x = as.numeric(age),
         y = value,
         fill = status_simple
       )) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  labs(x = "Age",
       y = "Number",
       fill = "") +
  scale_y_continuous(
    labels = scales::comma
  ) +
  facet_wrap(~iso, scales = "free_y") +
  theme(legend.position = c(.85, .75),
        legend.background=element_rect(fill = alpha("white", 0)),
        legend.key=element_rect(fill = alpha("white", .5))) +
  scale_fill_manual(values = cols)

ggsave("figs/presentations/20251205/MMR_vaccination_individual.jpg", width = 6, height = 4)



ggplot(data = suscept_complex_agg %>%
         subset(year == 2023 & iso == "MMR"),
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
  facet_wrap(~iso, scales = "free_y") +
  theme(legend.position = c(.85, .75),
        legend.background=element_rect(fill = alpha("white", 0)),
        legend.key=element_rect(fill = alpha("white", .5))) +
  scale_fill_manual(values = cols)

ggsave("figs/presentations/20251205/MMR_vaccination_and_exposure_individual.jpg", width = 6, height = 4)


pop_capita <- subset(suscept_complex_agg, year == 2023 & status == "Susceptible") %>%
  left_join(
  subset(population_all, year == 2023) %>%
    group_by(iso3) %>%
    mutate(total_population = 1000 * sum(across(x0:x100))) %>%
    select(iso = iso3, year, total_population)
) %>%
  group_by(iso) %>%
  summarise(value = sum(value),
            total_population = median(total_population)) %>%
  mutate(per_capita_susc = value/total_population * 1000)
  
#Per capita susceptible
ggplot(
  data = pop_capita,
  mapping = aes(
    y = iso,
    x = per_capita_susc
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    y = "",
    x = "Susceptibles (per 1000)"
  ) +
  scale_y_discrete(limits = rev)

ggsave("figs/presentations/20251205/susceptible_per_capita.jpg", width = 5, height = 3)


