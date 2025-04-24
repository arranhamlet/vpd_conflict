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


contact_matrix_raw <- contact_matricies$SDN





#Load in contact matrices


#Expand matrix



contact_matricies$AFG





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
