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
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = F), function(x) source(x)))

#Load in data
diff_fert_rates <- import(here("data", "raw_data", "crude_birth_rates_palestine.xlsx"))

gaza_prop_palestine_population <- 1#0.35
gaza_palestine_fertility_modifier <- median(diff_fert_rates$Gaza_Strip/diff_fert_rates$Palestine)

migration <- import(here("data", "raw_data", "palestine_WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.csv"))

five_year <- import(here("data", "raw_data", "five_year.csv"))

fertility <- import(here("data", "raw_data", "palestine_WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.csv"))

mortality_both <- import(here("data", "raw_data", "palestine_WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.csv"))
population_both <- import(here("data", "raw_data", "palestine_WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.csv")) 
population_female <- import(here("data", "raw_data", "palestine_WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.csv")) 

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

population_subset <- population_both %>%
  filter(Year >= 1964) %>%
  select(`0`:`100+`) * .35

#Deaths measured in 1000s
#Fertility measured in per 1000 women
#Population measured in 1000s


#Yearly changes
#Run for 60 years
time_run_for <- 60
time_all <- seq(0, time_run_for, by = 1)
data_change_here <- seq(0, time_run_for, by = 1)

#Work out mortality
mortality_change <- mortality_both %>%
  filter(Year >= 1964) %>%
  select(`0`:`100+`) 

mortality_correct_format <- mortality_change/population_subset
is.na(mortality_correct_format)<-sapply(mortality_correct_format, is.infinite)
mortality_correct_format[is.na(mortality_correct_format)]<-1
mortality_vector <- mortality_correct_format %>%
  as.matrix %>%
  t %>%
  c

#Need to convert to per day
mortality_vector <- pmin(mortality_vector, 1)

#Work out births
population_female_matched <- population_female %>%
  filter(Year >= 1964) %>%
  select(`15`:`49`) * 0.35 %>% 
  as.matrix
  
population_female_matched <- population_female_matched

fertility_matched <- fertility %>%
  filter(Year >= 1964) %>%
  select(`15`:`49`) %>%
  as.matrix

fertility_matched <- fertility_matched * gaza_palestine_fertility_modifier

population_all <- population_subset %>% 
  as.matrix %>%
  rowSums() %>%
  c

#Fertility calculations, divide by 2 assuming the population is 50:50 male/female (it isnt) and convert to daily
fertility_by_year <- rowSums((fertility_matched/1000 * population_female_matched))/rowSums(population_female_matched)

#Calculate migration
palestine_migration <- migration %>%
  filter(Year >= 1964) %>%
  select(`Net Migration Rate (per 1,000 population)`) %>%
  set_names("migration_per_1000") %>%
  c %>%
  unlist %>%
  as.numeric

#Work out migration by year
migration <- round(do.call(rbind, sapply(1:nrow(population_subset), function(x){
  population_subset[x,] * palestine_migration[x]/1000 
}, simplify = F)) * 1000, 0)

#Expanded grid
migration_expanded_grid <- do.call(rbind, sapply(1:60, function(x){
  data.frame(dim1 = x, dim2 = 1:101, dim3 = 1, dim4 = 1, value = as.numeric(migration[x, ]))
}, simplify = FALSE))



params <- param_packager(
  
  n_age = 101,
  n_vacc = 1,
  n_risk = 1,
  
  N0 = array(c(unlist(round(population_subset[1, ] * 1000, 0))), dim = c(101, 1, 1)),
  
  I0 = 0,
  initial_background_death = 0,
  aging_rate = 1,
  
  #Changing mortality and birth
  #Turn off simple birth/deaths
  simp_birth_death = 0,
  #List of when birth_death_changes
  tt_birth_changes = data_change_here,
  tt_death_changes = data_change_here,
  #Values of changes
  crude_birth = array(fertility_by_year, dim = c(length(data_change_here), 1)),
  crude_death = array(mortality_vector, dim = c(length(data_change_here), 101, 1)),
  #Birth ages
  repro_low = 15,
  repro_high = 49,
  
  #Migration
  tt_migration = data_change_here,
  migration_in_number = migration_expanded_grid,
  migration_distribution_values = 1
  
)

#Run model
time1 <- Sys.time()
clean_df <- run_model(
  params = params,
  time = time_run_for,
  no_runs = 2
)
time2 <- Sys.time()

time2 - time1

#Agg to year
population_over_time <- ggplot() +
  geom_line(
    data = subset(clean_df, run == "run_1" & state == "total_pop"),
    mapping = aes(
      x = 1964 + time,
      y = value * .35,
      color = "Model"
    )) +
  geom_line(
    data = data.frame(year = 1965:2024, value = 1000 * population_all),
    mapping = aes(
      x = year,
      y = value * .35, 
      color = "UN WPP"
    )
  ) + 
  scale_y_continuous(label = scales::comma) +
  labs(x = "Year",
         y = "Population",
       color = "",
       subtitle = paste0(paste0("Model population estimated in 2024: ", formatC(last(subset(clean_df, state == "total_pop")$value), big.mark = ",", format = "fg")), "\n",
                         paste0("UN WPP population estimated in 2024: ", formatC(last(population_all) * 1000, big.mark = ",", format = "fg")))) +
  theme_bw()

population_over_time

ggsave("figs/population_over_time_palestine_demographics.jpg", population_over_time, width = 5, height = 3)


#Plot histogram
age_pyramid_df <- rbind(
  
  data.frame(age = 1:101,
             value = subset(clean_df, run == "run_1" & state == "S" & time == 60 & age != "All") %>% mutate(age = as.numeric(age)) %>% pull(value),
             type = "Model estimate"),
  
  data.frame(age = 1:101,
             value = as.numeric(population_subset[60, ] * 1000),
             type = "UN WPP")
  
)

#Population pyramid
ggplot(mapping = aes(
  x = age,
  fill = type
)) +
  geom_histogram(
    data = subset(age_pyramid_df, type == "Model estimate"),
    mapping = aes( y = value),
    stat = "identity"
  ) +
  geom_histogram(
    data = subset(age_pyramid_df, type == "UN WPP"),
    mapping = aes(y = -1 * value),
    stat = "identity"
  ) +
  coord_flip() +
  theme_bw()

#Aggregate by five
age_pyramid_df_agg <- age_pyramid_df %>% 
  group_by(type, ceiling(age/5)) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  set_names("type", "age_group", "value") %>%
  mutate(age_group = factor(c(names(five_year), names(five_year)), levels = names(five_year))) %>%
  group_by(type) %>%
  mutate(age_group_prop = value/sum(value))


#Population pyramid
age_pyramid_df_agg

pop_pyramid <- ggplot(mapping = aes(
  x = age_group,
  fill = type
)) +
  scale_y_continuous(label = scales::comma, limits = c(-800000, 800000)) +
  geom_histogram(
    data = subset(age_pyramid_df_agg, type == "Model estimate"),
    mapping = aes( y = value),
    stat = "identity"
  ) +
  geom_histogram(
    data = subset(age_pyramid_df_agg, type == "UN WPP"),
    mapping = aes(y = -1 * value),
    stat = "identity"
  ) +
  coord_flip() +
  theme_bw() +
  labs(title = "Palestine population age pyramid in 2024",
       y = "",
       x = "",
       fill = "") +
  theme(legend.position = "bottom")

pop_pyramid

ggsave("figs/age_pyramid_palestine_demographics.jpg", pop_pyramid, width = 6, height = 4)




pop_pyramid_prop <- ggplot(mapping = aes(
  x = age_group,
  fill = type
)) +
  geom_histogram(
    data = age_pyramid_df_agg,
    mapping = aes( y = age_group_prop),
    stat = "identity",
    position = position_dodge()
  ) +
  theme_bw() +
  labs(title = "Proportion of Palestine population by age group (2024)",
       y = "",
       x = "",
       fill = "") +
  theme(legend.position = "bottom")

pop_pyramid_prop

ggsave("figs/age_pyramid_prop_palestine_demographics.jpg", pop_pyramid_prop, width = 8, height = 3)

