if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  dust2,
  tidyverse
)

#Load in model
model <- odin2::odin("models/mini_model.R")

#Set up parameters
pars <- list(
  N = matrix(100, nrow = 1, ncol = 1),
  birth_rate = 0.25
)

#Define dust system and initialise
sys <- dust2::dust_system_create(model(), pars)
dust2::dust_system_set_state_initial(sys)

#Set time
time <- 0:100
#Run model
y <- dust2::dust_system_simulate(sys, time)

#Outputs
sum(y)
