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
  ggforce,
  ggpubr,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import model
WHO_cases <- import("data/processed/WHO/reported_cases_data.csv") %>%
  subset(iso3 == "GBR" & disease_description == "Measles") %>%
  select(year, cases) %>%
  mutate(source = "WHO")
UKHSA_cases <- import("data/raw/cases/UK_measles_cases_1940_2023.xlsx") %>%
  select(year = Year,
         cases = Notifications) %>%
  mutate(source = "UKHSA")

combo_cases <- rbind(
  WHO_cases,
  UKHSA_cases
)

#Plot
ggplot(
  data = combo_cases,
  mapping = aes(
    x = year,
    y = cases,
    fill = source
  )
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  labs(
    x = "",
    y = "Cases",
    fill = ""
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = c(0.8, 0.7)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))
  
ggsave("figs/presentations/20251205/UKHSA_WHO_compare_measles.jpg", height = 3, width = 6)




ggplot(
  data = combo_cases %>% subset(year >= 2000),
  mapping = aes(
    x = year,
    y = cases,
    fill = source
  )
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  labs(
    x = "",
    y = "Cases",
    fill = ""
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = c(0.8, 0.7)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))

ggsave("figs/presentations/20251205/UKHSA_WHO_compare_measles_2000.jpg", height = 3, width = 6)


model_data <- list.files("output/model_run/WHO_showcase", pattern = "full_data", full.names = T)
model_data <- model_data[grepl("GBR", model_data)]
all_full_data <- import(model_data)

susc_data <- list.files("output/model_run/WHO_showcase", pattern = "susceptibility_data", full.names = T)
susc_data <- susc_data[grepl("GBR", susc_data)]
susc_data_full <- import(susc_data)

#Case over time
first_part <- ggplot(
) +
  geom_line(data = all_full_data %>%
              subset(age == "All" & state == "new_case" & year >= 1960 & year < 2024),
            mapping = aes(
              x = year,
              y = value
            )) +
  geom_ribbon(data = all_full_data %>%
                subset(age == "All" & state == "new_case" & year >= 1960 & year < 2024),
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
    fill = ""
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  # facet_zoom(xy = year >= 2000 & year <= 2023, zoom.size = 1, horizontal = F) +
  geom_bar(
    data = combo_cases %>% subset(year >= 1960),
    mapping = aes(
      x = year,
      y = cases,
      fill = source
    ),
    position = position_dodge(),
    stat = "identity"
  ) +
  theme(legend.position = c(0.8, 0.7))
  

ggsave("figs/presentations/20251205/UKHSA_WHO_model_compare_measles.jpg", first_part, height = 3, width = 6)


second_part <- ggplot(
) +
  geom_line(data = all_full_data %>%
              subset(age == "All" & state == "new_case" & year >= 2000 & year < 2024),
            mapping = aes(
              x = year,
              y = value
            )) +
  geom_ribbon(data = all_full_data %>%
                subset(age == "All" & state == "new_case" & year >= 2000 & year < 2024),
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
    fill = ""
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  # facet_zoom(xy = year >= 2000 & year <= 2023, zoom.size = 1, horizontal = F) +
  geom_bar(
    data = combo_cases %>% subset(year >= 2000),
    mapping = aes(
      x = year,
      y = cases,
      fill = source
    ),
    position = position_dodge(),
    stat = "identity"
  ) +
  theme(legend.position = c(0.8, 0.7))

ggsave("figs/presentations/20251205/UKHSA_WHO_model_compare_measles_2000.jpg", second_part, height = 3, width = 6)


#Susceptibility plot
susc_agg <- susc_data_full %>%
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



cols <- c("Susceptible" = "#F8766D", "Vaccine protected" = "#00BFC4", "Exposure protected" = "#7CAE00", "Vaccine and exposure protected" = "#C77CFF")


ggplot(data = susc_agg %>%
         subset(year == 2023),
       mapping = aes(
         x = as.numeric(age),
         y = value,
         fill = status_vacc
       )) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  labs(x = "Age",
       y = "Number",
       fill = "") +
  scale_y_continuous(
    labels = scales::comma
  ) +
  theme(legend.position = c(.85, .85),
        legend.background=element_rect(fill = alpha("white", 0)),
        legend.key=element_rect(fill = alpha("white", .5))) +
  scale_fill_manual(values = cols)

ggsave("figs/presentations/20251205/GBR_vaccination_individual.jpg", width = 8, height = 4)



ggplot(data = susc_agg %>%
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
  theme(legend.position = c(.85, .85),
        legend.background=element_rect(fill = alpha("white", 0)),
        legend.key=element_rect(fill = alpha("white", .5))) +
  scale_fill_manual(values = cols)

ggsave("figs/presentations/20251205/GBR_vaccination_and_exposure_individual.jpg", width = 8, height = 4)

#Total susceptible
tot_sus_simp <- susc_agg %>%
  subset(year == 2023) %>%
  group_by(status_vacc) %>%
  summarise(value = sum(value)) %>%
  mutate(prop = value/sum(value))

tot_sus_comp <- susc_agg %>%
  subset(year == 2023) %>%
  group_by(status) %>%
  summarise(value = sum(value)) %>%
  mutate(prop = value/sum(value))


#Loop Loop Loop



probability_things <- sapply(countries_of_interest, function(a){

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



  }, simplify = FALSE)
