if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  rio,
  here,
  tidyverse,
  reshape2,
  collapse,
  janitor,
  patchwork,
  data.table,
  tidymodels
)

#Load in contact matricies
contact_matricies <- import("data/raw/contact_matricies/contact_all.rdata")
age_group <- seq(5, 80, by = 5)

melted_df <- reshape2::melt(contact_matricies$AFG) %>%
  rename(age_one = Var1,
         age_two = Var2) %>%
  mutate(age_one = age_group[age_one],
         age_two = age_group[age_two])

#Regression
lm_mod <- linear_reg() %>% 
  set_engine("keras")

lm_fit <- lm_mod %>% 
  fit(value ~ age_one + age_two, data = melted_df)

#Extrapolate model
map_data <- expand.grid(age_one = 0:80,
                        age_two = 0:80)
new_data <- predict(
  lm_fit,
  newdata = map_data
)

map_data$value <- new_data

#Plot
old_plot <- ggplot(
  data = melted_df,
  mapping = aes(
    x = age_one,
    y = age_two,
    fill = value
  )
) +
  geom_tile() +
  labs(x = "",
       y = "",
       fill = "") +
  theme_bw()

new_plot <- ggplot(
  data = map_data,
  mapping = aes(
    x = age_one,
    y = age_two,
    fill = value
  )
) +
  geom_tile() +
  labs(x = "",
       y = "",
       fill = "") +
  theme_bw()

old_plot + new_plot
