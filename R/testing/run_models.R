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
  n_vulnerable = 2,
  
  #Contact matrix
  contact_matrix = matrix(1, nrow = 2, ncol = 2)/4,
  
  #Single dimension parameters
  R0 = 1.5,
  recovery_rate = 1/14,
  incubation_rate = 1/5,
  severe_recovery_rate = 1/14,
  prop_complications = 0,
  
  #Multi dimension parameters
  N0 = array(c(0, 999, 0, 0, 0, 0, 0, 0), dim = c(2, 2, 2)),
  I0 = array(c(0, 1, 0, 0, 0, 0, 0, 0), dim = c(2, 2, 2)),
  prop_severe = array(c(0, 0, 0, 0, 0, 0, 0, 0), dim = c(2, 2, 2)),
  age_vaccination_beta_modifier = array(c(1, 1, 1, 1, 1, 1, 1, 1), dim = c(2, 2, 2)),
  
  # N0 = array(c(0, 1000, 0, 0), dim = c(2, 2, 1)),
  # I0 = array(c(0, 0, 0, 0), dim = c(2, 2, 1)),
  # prop_severe = array(c(0, 0, 0, 0), dim = c(2, 2, 1)),
  # age_vaccination_beta_modifier = array(c(1, 1, 1, 1), dim = c(2, 2, 1)),
  
  # birth_rate = 0.1,
  background_death = 0.01,#0.01,
  
  #Aging
  aging_rate = c(0.02, 0),
  
  #Maternal immunity waning
  protection_weight = 0,
  age_maternal_protection_ends = 1,
  
  #Reproductive ages
  repro_low = 2,
  repro_high = 2
  
)

#Something happening in E and I that is making people disappear
#it appears to be in the background death rate
#Happening in S->E
#Some shennanigans are causing the population to be reduced


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
    vulnerable_population = c("Standard risk", "E"),
    time = list(time)
  ),
  which_state_dimensions = list(
    S = c("age", "vaccination_status", "vulnerable_population", "time"),
    E = c("age", "vaccination_status", "vulnerable_population", "time"),
    I = c("age", "vaccination_status", "vulnerable_population", "time"),
    R = c("age", "vaccination_status", "vulnerable_population", "time"),
    Is = c("age", "vaccination_status", "vulnerable_population", "time"),
    Rc = c("age", "vaccination_status", "vulnerable_population", "time"),
    M_protected = c("vulnerable_population", "time")
  )
)

#Specific plot
#Plot
ggplot(
  data = subset(clean_df, (vulnerable_population == "Standard risk" & vaccination_status == "Unvaccinated" & age %in% c("Child", "Adult")) | state %in% c("pop", "M_protected", "aging_into_two", "lamb", "infy", "beta1", "beta2")),
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
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "",
    color = ""
  )

# 

ggplot(
  data = subset(clean_df, age == "All" | state %in% c("pop", "reff", "born", "died")),
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
    legend.position = c(0.75, 0.125)
  ) +
  labs(
    x = "",
    y = "",
    color = ""
  )
