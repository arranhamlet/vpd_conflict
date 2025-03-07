if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  dust2,
  tidyverse
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/mini_model.R")

#Set up parameters
pars <- list(
  
  #Dimensions
  n_age = 2,
  n_vacc = 1,
  
  #Single dimension parameters
  R0 = 1.5,
  recovery_rate = 1/14,
  incubation_rate = 1/5,
  b = 0,
  severe_recovery_rate = 1/14,
  prop_complications = 0.1,
  
  #Multi dimension parameters
  N0 = matrix(c(750, 250), nrow = 2, ncol = 1),
  I0 = matrix(c(1, 0), nrow = 2, ncol = 1),
  prop_severe = matrix(c(0.1, .1), nrow = 2, ncol = 1),
  age_vaccination_beta_modifier = matrix(c(1, 1), nrow = 2, ncol = 1)
  
)

#Define dust system and initialise
sys <- dust2::dust_system_create(model(), pars)
dust2::dust_system_set_state_initial(sys)

#Set time and run model
time <- 0:700
y <- dust2::dust_system_simulate(sys, time)

#Unpack columns
clean_df <- unpack_dust2(
  model_system = sys, 
  model_object = y, 
  dimension_names = list(
    age = list("Child", "Adult"), 
    vaccination_status = "Unvaccinated", 
    time = list(0:700)
  )
)

#Plot
ggplot(
  data = subset(clean_df, !age %in% c("Child", "Adult")),
  mapping = aes(
    x = time,
    y = value,
    color = age
  )
) +
  geom_point() +
  facet_wrap(
    ~state,
    scales = "free_y"
  )











# 
# #Unpack columns
# clean_df <- unpack_dust2(
#   model_system = sys, 
#   model_object = y, 
#   dimension_names = list(
#     age = list("All"), 
#     vaccination_status = "Unvaccinated", 
#     time = list(0:700)
#   )
# )
# 
# 
# 
# 
# 
# pars <- list(
#   
#   #Dimensions
#   n_age = 1,
#   n_vacc = 1,
#   
#   #Single dimension parameters
#   R0 = 1.5,
#   recovery_rate = 1/14,
#   incubation_rate = 1/5,
#   b = 0,
#   severe_recovery_rate = 1/14,
#   prop_complications = 0.1,
#   
#   #Multi dimension parameters
#   N0 = matrix(c(1000), nrow = 1, ncol = 1),
#   I0 = matrix(c(1), nrow = 1, ncol = 1),
#   prop_severe = matrix(c(0.1), nrow = 1, ncol = 1),
#   age_vaccination_beta_modifier = matrix(c(1), nrow = 1, ncol = 1)
#   
# )
# 



