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
  ggpubr,
  squire.page,
  patchwork,
  patchwork,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Load WHO disease data
cases_of_interest <- import("data/processed/WHO/reported_cases_data.csv") %>%
  subset(disease_description %in% c("Pertussis", "Measles", "Diphtheria") & iso3 %in% "GBR")  %>%
  mutate(disease_description = tolower(disease_description))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Run through diseases of interest
diseases <- data.frame(
  disease = c("measles", "pertussis", "diphtheria"),
  R0 = c(15, 12, 3)
)

#Loop
all_run <- sapply(1:nrow(diseases), function(x){
  
  print(x)
  
  #Run process
  model_data_processed <- data_load_process_wrapper(
    iso = "GBR",
    disease = diseases$disease[x],
    vaccine = diseases$disease[x],
    R0 = diseases$R0[x],
    timestep = "week",
    WHO_seed_switch = T
  )
  
  #Process for plotting
  model_ran <- run_model(
    odin_model = model,
    params = model_data_processed$params,
    time = floor(model_data_processed$time),
    no_runs = 4
  )
  
  process_for_plotting(model_ran, input_data = model_data_processed$input_data)
  
}, simplify = FALSE)

# Combine and compare
full_df <- Reduce(rbind, sapply(1:length(all_run), function(x) all_run[[x]][[1]] %>%
                                  mutate(disease = diseases$disease[x]), simplify = FALSE))

full_susceptibility<- Reduce(rbind, sapply(1:length(all_run), function(x) all_run[[x]][[2]] %>%
                                  mutate(disease = diseases$disease[x]), simplify = FALSE))

#Plot cases
full_df <- full_df %>%
  left_join(
    cases_of_interest %>%
      select(disease_description, year, cases),
    by = c("disease" = "disease_description", "year" = "year")
  )

#Plot
full_time <- ggplot() +
  geom_line(
    data = full_df %>%
      subset(state == "new_case" & age == "All" & year >= 1956),
    mapping = aes(
      x = year,
      y = value,
      color = str_to_sentence(disease)
    )
  ) +
  geom_ribbon(
    data = full_df %>%
      subset(state == "new_case" & age == "All" & year >= 1956),
    mapping = aes(
      x = year,
      y = value,
      ymin = value_min,
      ymax = value_max,
      fill = str_to_sentence(disease)
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(x = "",
       y = "Annual cases",
       color = "",
       fill = "") +
  theme(legend.position = c(0.75, 0.7))

last_25 <- ggplot() +
  geom_line(
    data = full_df %>%
      subset(state == "new_case" & age == "All" & year >= 2000),
    mapping = aes(
      x = year,
      y = value,
      color = str_to_sentence(disease)
    )
  ) +
  geom_ribbon(
    data = full_df %>%
      subset(state == "new_case" & age == "All" & year >= 2000),
    mapping = aes(
      x = year,
      y = value,
      ymin = value_min,
      ymax = value_max,
      fill = str_to_sentence(disease)
    )
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge(),
    data = full_df %>%
      subset(state == "new_case" & age == "All" & year >= 2000),
    mapping = aes(
      x = year,
      y = cases,
      fill = str_to_sentence(disease)
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(x = "",
       y = "",
       color = "",
       fill = "") +
  theme(legend.position = c(0.75, 0.7))

cases_save <- ggarrange(full_time, last_25, common.legend = T, legend = "bottom", widths = c(2, 1))
ggsave("figs/per_dip_mea_cases.jpg", cases_save, height = 3, width = 9)



#Compare vaccine immunity
susc_agg <- full_susceptibility %>%
  group_by(age, year, status, disease) %>%
  summarise(
    value = sum(value),
    value_max = sum(value_max),
    value_min = sum(value_min),
    .groups = "drop"
  ) %>%
  group_by(age, year, disease) %>%
  mutate(prop = value/sum(value)) %>%
  mutate(status_vacc = case_when(
    grepl("vaccine", status, ignore.case = T) ~ "Vaccine protected",
    !grepl("vaccine", status, ignore.case = T) ~ "Susceptible"
  ))

# Exposure
protection_2023 <- ggplot(
  data = subset(susc_agg, year == 2023),
  mapping = aes(
    x = as.numeric(age),
    y = prop,
    fill = status
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "", y = "Proportion of the population", fill = "", title = "Age-specific protection levels in 2023") +
  facet_wrap(~disease) +
  scale_y_continuous() +
  theme(legend.position = "bottom")

ggsave("figs/protection_2023_3_diseases_upd.jpg", height = 4, width = 7)















