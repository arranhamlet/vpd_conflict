if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  dust2,
  tidyverse,
  reshape2,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Get parameters
params <- param_packager(
 
  n_age = 2,
  n_vacc = 2,
  n_vulnerable = 1,
  
  incubation_rate = 0.2,
  recovery_rate = 1/14,
  R0 = 2,
  initial_background_death = 0.001,
  aging_rate = 0.005,
  I0 = 0,
  N0 = 100,
  
  #Seeding parameters
  seeded = c(0, 0, 0),
  #Define times when seeding occurs
  tt_seeded = c(0, 400, 401),
  
  #Check time varying vaccination
  vaccination_coverage = array(c(0, 0, 0.25, 0.25, 0, 0), dim = c(3, 1, 2, 1)),
  tt_vaccination_coverage = c(0, 200, 201),
  waning_rate = 0.1

)

#Define dust system and initialise
sys <- dust2::dust_system_create(model(), params, n_particles = 10)
dust2::dust_system_set_state_initial(sys)

#Set time and run model
time <- 0:800
y <- dust2::dust_system_simulate(sys, time)

#Clean output
#Unpack columns
clean_df <- unpack_dust2(
  model_system = sys, 
  model_object = y, 
  dimension_names = list(
    age = list(as.character(1:params$n_age)), 
    vaccination_status = list(as.character(1:params$n_vacc)),
    vulnerable_population = list(as.character(1:params$n_vulnerable)),
    time = list(time)
  ),
  which_state_dimensions = list(
    S = c("age", "vaccination_status", "vulnerable_population", "time"),
    E = c("age", "vaccination_status", "vulnerable_population", "time"),
    I = c("age", "vaccination_status", "vulnerable_population", "time"),
    R = c("age", "vaccination_status", "vulnerable_population", "time"),
    Is = c("age", "vaccination_status", "vulnerable_population", "time"),
    Rc = c("age", "vaccination_status", "vulnerable_population", "time")
  )
)

#Simple test plot
ggplot(data = subset(clean_df, state == "S" & age != "All"),
       mapping = aes(
         x = time,
         y = value,
         color = age,
         linetype = vaccination_status
         # group_by = age
       )) +
  geom_line() +
  facet_wrap(~run, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    x = "",
    y = ""
  )

##############


ggplot(data = subset(clean_df, age == "All" & run == "run_4"),
       mapping = aes(
         x = time,
         y = value,
         color = run,
         group_by = age
       )) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    x = "",
    y = ""
  )
