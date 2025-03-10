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
  n_vacc = 2,
  
  #Contact matrix
  contact_matrix = matrix(1, nrow = 2, ncol = 2)/4,
  
  #Single dimension parameters
  R0 = 1.5,
  recovery_rate = 1/14,
  incubation_rate = 1/5,
  b = 0.125,
  severe_recovery_rate = 1/14,
  prop_complications = 0.1,
  
  #Multi dimension parameters
  N0 = matrix(c(0, 1000, 0, 0), nrow = 2, ncol = 2),
  I0 = matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2),
  prop_severe = matrix(0, nrow = 2, ncol = 2),
  age_vaccination_beta_modifier = matrix(1, nrow = 2, ncol = 2),
  
  #Aging
  aging_rate = c(.125, 0),
  
  #Reproductive ages
  repro_low = 2,
  repro_high = 2
  
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
    vaccination_status = c("Unvaccinated", "Vaccinated"), 
    time = list(0:700)
  )
)

#Plot
ggplot(
  data = subset(clean_df, age %in% c("Child", "Adult") ),
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
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = ""
  )


#Specific plot
#Plot
ggplot(
  data = subset(clean_df, (vaccination_status == "Unvaccinated" & age %in% c("Child", "Adult")) | state == "pop" ),
  mapping = aes(
    x = time,
    y = value,
    color = age
  )
) +
  geom_line() +
  facet_wrap(
    ~state,
    scales = "free_y"
  ) +
  theme(
    legend.position = c(0.5, 0.125)
  ) +
  labs(
    x = "",
    y = "",
    color = ""
  )

