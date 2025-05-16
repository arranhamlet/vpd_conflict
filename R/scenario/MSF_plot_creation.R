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
  patchwork,
  squire.page,
  patchwork,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#UN demographics
population_all <- import(here("data", "processed", "WPP", "age_both.csv"))
population_NGA <- subset(population_all, iso3 == "NGA") %>% 
  select(x0:x100) %>% 
  tail(1) %>%
  gather() %>%
  rename(age = key, population = value) %>%
  mutate(age = as.numeric(gsub("x", "", age)),
         population = population * 1000)

#Prior vaccination coverage
routine_vaccination_data <- import("data/raw/WHO/coverage-data_updated.xlsx")
routine_subset <- routine_vaccination_data %>%
  subset(CODE == "NGA" & 
           grepl("measles", ANTIGEN_DESCRIPTION, ignore.case = T) & COVERAGE_CATEGORY == "WUENIC") %>%
  clean_names() %>%
  select(code, name, year, antigen_description, coverage)

#Full case data
prior_cases <- import("output/model_run/MSF/processed/full_cases.csv")

#Full starting immunity
starting_immunity <- import("output/model_run/MSF/processed/susceptibility.csv")
starting_immunity <- starting_immunity %>%
  mutate(status = factor(
    status, levels = c("Susceptible", "Vaccine protected", "Exposure protected", "Vaccine and exposure protected")
  ))

#Rdata files
all_Rdata <- list.files("output/model_run/MSF/processed/", pattern = ".rds", full.names = T)
all_Rdata_names <- sapply(strsplit(all_Rdata, "/"), function(e) gsub(".rds", "", last(e)))
all_Rdata_loaded <- sapply(all_Rdata, function(x) import(x), simplify = FALSE)
names(all_Rdata_loaded) <- all_Rdata_names

#Now plot
NGA_demographics <- ggplot(
  data = population_NGA,
  mapping = aes(x = age,
                y = population)
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Age",
       y = "Population") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 16)
  )


NGA_measles <- ggplot(
  data = prior_cases %>%
    subset(iso == "NGA" & disease == "measles" & year >= 1980),
  mapping = aes(
    x = year,
    y = value,
    ymin = value_min,
    ymax = value_max
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "",
       y = "Annual cases") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 16)
  )

NGA_measles

NGA_vaccination <- ggplot(
  data = routine_subset,
  mapping = aes(
    x = year,
    y = coverage,
    color = antigen_description
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "Coverage (%)",
    color = ""
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.85),
    legend.background = element_rect(fill = NA,
                                     linetype = "solid", 
                                     color = NA)
  ) +
  coord_cartesian(
    ylim = c(0, 100)
  ) +
  theme(
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

susceptibility <- ggplot(
  data = starting_immunity %>%
    subset(iso == "NGA" & disease == "measles") %>%
    mutate(status = gsub("and", "and\n", status),
           status = factor(status,
                           levels = c("Exposure protected",
                                      "Susceptible",
                                      "Vaccine protected",
                                      "Vaccine and\n exposure protected"))),
  mapping = aes(
    x = as.numeric(age),
    y = prop,
    fill = status
  )
) +
  geom_bar(
    stat = "identity",
    width = 1
  ) +
  theme_bw() +
  labs(
    x = "Age",
    y = "Proportion",
    fill = ""
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.5),
    legend.background = element_rect(fill = NA,
                                     linetype = "solid", 
                                     color = NA),
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))



patch <- NGA_demographics/ NGA_measles/ NGA_vaccination

total_information <- ggarrange(patch, susceptibility, ncol = 2, widths = c(1, 1.25))


ggsave("figs/presentations/MSF/total_information.jpg", total_information, height = 7, width = 14)

#Run Nigeria at different levesl
param_use <- import("output/model_run/MSF/processed/NGA_measles_15.rds")

generate_new_params <- sapply(c(1, 2, 3), function(x){
  
  
  new_vaccination <- abind::abind(
    param_use$vaccination_coverage,
    param_use$vaccination_coverage,
    param_use$vaccination_coverage,
    param_use$vaccination_coverage,
    param_use$vaccination_coverage,
    along = 4
  )
  
  if(x == 2){
    new_vaccination[, 1:3, 1, 1] <- new_vaccination[, 1:3, 1, 1]  * 0.25
    new_vaccination[, 1:3, 1, 2] <- 0
    new_vaccination[, 1:3, 1, 3] <- 0
    new_vaccination[, 1:3, 1, 4] <- 0
    new_vaccination[, 1:3, 1, 5] <- 0
  }
  
  if(x == 3){
    new_vaccination[, 1:3, 1, 1] <- new_vaccination[, 1:3, 1, 1]  * 0.25
    new_vaccination[, 1:3, 1, 2] <- 0
    new_vaccination[, 1:3, 1, 3] <- param_use$vaccination_coverage[, 1:3, 1, 1]
    new_vaccination[, 1:3, 1, 4] <- param_use$vaccination_coverage[, 1:3, 1, 1]
    new_vaccination[, 1:3, 1, 5] <- param_use$vaccination_coverage[, 1:3, 1, 1]
  }
  
  param_use$vaccination_coverage <- new_vaccination
  param_use$tt_vaccination_coverage <- seq(0, by = 365, length.out = 5)
  param_use$no_vacc_changes <- length(param_use$tt_vaccination_coverage)
  
  
  new_seeded <- abind::abind(
    param_use$seeded,
    param_use$seeded,
    param_use$seeded,
    param_use$seeded,
    param_use$seeded,
    along = 4
  )
  
  new_seeded[18, 1, 1, 1] <- 0
  new_seeded[18, 1, 1, 2] <- 0
  new_seeded[18, 1, 1, 3] <- 0
  new_seeded[18, 1, 1, 4] <- 1
  new_seeded[18, 1, 1, 5] <- 0
  
  param_use$tt_seeded <- c(0, 
                           366, 367,
                           1095, 1096)
  param_use$seeded <- new_seeded
  param_use$no_seeded_changes <- 5
  
  total_pop <- sum(param_use$S0 + param_use$I0 + param_use$Rpop0)
  simulated_pop <- 100000
  scaler <- simulated_pop/total_pop
  
  param_use$S0 <- param_use$S0 * scaler
  param_use$I0 <- param_use$I0 * scaler
  param_use$Rpop0 <- param_use$Rpop0 * scaler
  
  param_use
  
}, simplify = FALSE)

#Run model
model_no_change <- run_model(
  odin_model = model,
  params = generate_new_params[[1]],
  time = 365 * 4,
  no_runs = 10
)
model_no_change$version <- "No change"

model_change <- run_model(
  odin_model = model,
  params = generate_new_params[[2]],
  time = 365 * 4,
  no_runs = 10
)
model_change$version <- "Reduction in coverage"

model_better <- run_model(
  odin_model = model,
  params = generate_new_params[[3]],
  time = 365 * 4,
  no_runs = 10
)
model_better$version <- "Increase coverage"

combo <- rbind(
  model_no_change,
  model_change,
  model_better
) 

#Calculate susceptibility
new_cases_go <- combo %>%
  subset(age == "All" & state == "new_case") %>%
  fgroup_by(time, version) %>%
  fsummarise(
    value = median(value),
    value_min = get_95CI(x = value, type = "low"),
    value_max = get_95CI(x = value, type = "high")
  ) %>%
  mutate(
    value_min = pmax(value_min, 0)
  )

#Summary statistics
sum_stats <- combo %>%
  subset(age == "All" & state == "new_case") %>% 
  group_by(run, version) %>%
  summarise(value = sum(value)) %>%
  mutate(over_100 = value > 100)

sum_stats_outbreak_over_100 <- sum_stats %>%
  group_by(version) %>%
  summarise( n = n(),
             over_100 = sum(over_100)) %>%
  mutate(less_than_100 = n - over_100,
         prop_diff = less_than_100/min(less_than_100))

sum_stats_size <- sum_stats %>%
  group_by(version) %>%
  summarise(
    value_min = get_95CI(x = value, type = "low"),
    value_max = get_95CI(x = value, type = "high"),
    value = mean(value),
  )

paste(max(sum_stats_size$value)/min(sum_stats_size$value),
       max(sum_stats_size$value_min)/min(sum_stats_size$value_min),
       max(sum_stats_size$value_max)/min(sum_stats_size$value_max))


case_diff <- ggplot(
  data = sum_stats_size %>%
    subset(version != "Increase coverage"),
  mapping = aes(
    x = version,
    y = value,
    ymin = value_min,
    ymax = value_max,
    color = version
  )
) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(
    x = "",
    y = "Cases",
    color = ""
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("No change" = "gray30",
                                  "Reduction in coverage" = "tomato1")) +
  theme(legend.position = "none",
        axis.text.x = element_blank()) 

ggsave("figs/presentations/MSF/case_difference.jpg", height = 3, width = 4)


#Quick check
outbreak_plot <- ggplot(
  data = new_cases_go,
  mapping = aes(
    x = time,
    y = value,
    ymin = value_min,
    ymax = value_max,
    color = version,
    fill = version
  ) 
) +
  geom_line(linewidth = 2) +
  geom_ribbon(alpha = 0.25) +
  theme_bw() +
  labs(x = "Days",
       y = "Cases",
       fill = "Scenario",
       color = "Scenario") +
  coord_cartesian(
    xlim = c(1095, 1350) 
  ) +
  geom_vline(xintercept = 1095,
             linetype = "dashed") +
  geom_text(mapping = aes(
    x = 1095 + 25,
    y = 500
  ), label = "Introduction",
  color = "black") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.75, .4)
  ) +
  scale_color_manual(values = c("No change" = "gray30",
                                "Reduction in coverage" = "tomato1")) +
  scale_fill_manual(values = c("No change" = "gray30",
                                "Reduction in coverage" = "tomato1"))

outbreak_plot +
  inset_element(case_diff, 0.5, 0.5, 0.95, .95)

ggsave("figs/presentations/MSF/outbreak_plot.jpg", height = 4, width = 6)


susceptibility_data_all <- subset(combo, state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All") %>%
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
    time, version, status
  ) %>%
  summarise(
    value = sum(value),
    .groups = "drop"
  ) %>%
  group_by(
    time, version
  ) %>%
  mutate(
    coverage = value/sum(value, na.rm = T),
    coverage = case_when(
      is.nan(coverage) ~ 0,
      !is.nan(coverage) ~ coverage
    )
  )

susceptibility_data_under_18 <- subset(combo, state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All") %>%
  subset(age <= 18) %>%
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
    time, version, status
  ) %>%
  summarise(
    value = sum(value),
    .groups = "drop"
  ) %>%
  group_by(
    time, version
  ) %>%
  mutate(
    coverage = value/sum(value, na.rm = T),
    coverage = case_when(
      is.nan(coverage) ~ 0,
      !is.nan(coverage) ~ coverage
    )
  )


#Over the time period susceptibility to infection will increase by:
#Overall population: 20%
#Under 5: %


#Plot
vac_protect_all <- susceptibility_data_all %>%
  mutate(status_simple = case_when(
    grepl("Vaccine", status) ~ "Vaccinated",
    !grepl("Vaccine", status) ~ "Non-vaccinated"
  )) %>%
  group_by(time, version, status_simple) %>%
  summarise(value = sum(value)) %>%
  group_by(time, version) %>%
  mutate(prop = value/sum(value))


percent_vaccinated <- ggplot(
  data = vac_protect_all %>%
    subset(version == "Reduction in coverage") %>%
    mutate(year = time/365),
  mapping = aes(
    x = time,
    y = prop * 100,
    fill = status_simple
  )
) +
  geom_bar(stat = "identity",
           width = 1) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.25),
    legend.background = element_rect(fill = NA,
                                     linetype = "solid", 
                                     linewidth = 1),
    axis.text = element_text(size = 14), 
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "",
       x = "Days",
       y = "% of population") +
  coord_cartesian(ylim = c(40, 50)) +
  geom_hline(
    yintercept = subset(vac_protect, status_simple == "Vaccinated" & version == "Reduction in coverage" & time == 1) %>% pull(prop) * 100,
    linetype = "dashed"
  )




vac_protect_u18 <- susceptibility_data_under_18 %>%
  mutate(status_simple = case_when(
    grepl("Vaccine", status) ~ "Vaccinated",
    !grepl("Vaccine", status) ~ "Non-vaccinated"
  )) %>%
  group_by(time, version, status_simple) %>%
  summarise(value = sum(value)) %>%
  group_by(time, version) %>%
  mutate(prop = value/sum(value))


ggplot(
  data = vac_protect_u18 %>%
    subset(version == "Reduction in coverage") %>%
    mutate(year = time/365),
  mapping = aes(
    x = time,
    y = prop * 100,
    fill = status_simple
  )
) +
  geom_bar(stat = "identity",
           width = 1) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.25),
    legend.background = element_rect(fill = NA,
                                     linetype = "solid", 
                                     linewidth = 1),
    axis.text = element_text(size = 14), 
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "",
       x = "Days",
       y = "% of population") +
  coord_cartesian(ylim = c(60, 70)) +
  geom_hline(
    yintercept = subset(vac_protect, status_simple == "Vaccinated" & version == "Reduction in coverage" & time == 1) %>% pull(prop) * 100,
    linetype = "dashed"
  )













ggplot(
  data = vac_protect_all %>%
    subset(version == "Increase coverage") %>%
    mutate(year = time/365),
  mapping = aes(
    x = time,
    y = prop * 100,
    fill = status_simple
  )
) +
  geom_bar(stat = "identity",
           width = 1) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.25),
    legend.background = element_rect(fill = NA,
                                     linetype = "solid", 
                                     linewidth = 1),
    axis.text = element_text(size = 14), 
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "",
       x = "Days",
       y = "% of population") +
  # coord_cartesian(ylim = c(40, 50)) +
  geom_hline(
    yintercept = subset(vac_protect, status_simple == "Vaccinated" & version == "Reduction in coverage" & time == 1) %>% pull(prop) * 100,
    linetype = "dashed"
  )


