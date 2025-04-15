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

contact_matricies <- import("data/raw/contact_matricies/contact_all.rdata")
contact_matrix_raw <- contact_matricies$SDN

reformat_contact_matrix <- function(
    contact_matrix_raw
    ){
  
  #Names
  age_group <- seq(0, 80, by = 5)
  
  #Rename columns
  row.names(contact_matrix_raw) <- age_group[2:length(age_group)]
  colnames(contact_matrix_raw) <- age_group[2:length(age_group)]
  
  #WPP data is 0-100, create a matrix and fill in values
  contact_matrix_expanded <- matrix(0, nrow = 101, ncol = 101)
  
  for(i in 1:nrow(contact_matrix_expanded)){
    for(j in 1:ncol(contact_matrix_expanded)){
      age_group
    }
  }
  
  
}

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
