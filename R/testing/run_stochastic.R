if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  dust2,
  tidyverse,
  reshape2,
  collapse,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Get parameters
params <- param_packager(
 
  n_age = 1,
  n_vacc = 2,
  n_risk = 1,
  
  incubation_rate = 0.2,
  recovery_rate = 1/14,
  R0 = 10,
  initial_background_death = 0.001,
  aging_rate = 0.005,
  I0 = 0,
  N0 = 100,
  severe_recovery_rate = 1/14,
  prop_severe = 0.1,
  prop_complications = 0.1,
  
  #Seeding parameters
  seeded = array(c(0, 0, 1, 0, 0, 0), dim = c(3, 1, 2, 1)),
  tt_seeded = c(0, 200, 201)
  
  #Check time varying vaccination
  # vaccination_coverage = array(c(0, 0, 0.25, 0.25, 0, 0), dim = c(3, 1, 2, 1)),
  # tt_vaccination_coverage = c(0, 200, 201),
  # waning_rate = 0.0001

)

#Define dust system and initialise
sys <- dust2::dust_system_create(model(), params, n_particles = 100)
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
    vaccination = list(as.character(1:params$n_vacc)),
    risk = list(as.character(1:params$n_risk)),
    time = list(time)
  ),
  which_state_dimensions = list(
    S = c("age", "vaccination", "risk", "time"),
    E = c("age", "vaccination", "risk", "time"),
    I = c("age", "vaccination", "risk", "time"),
    R = c("age", "vaccination", "risk", "time"),
    Is = c("age", "vaccination", "risk", "time"),
    Rc = c("age", "vaccination", "risk", "time")
  )
)

# Simple test plot
# ggplot(data = subset(clean_df, age == "All" & state == "S"),
#        mapping = aes(
#          x = time,
#          y = value,
#          color = age,
#          linetype = vaccination
#        )) +
#   geom_line() +
#   facet_wrap(~run, scales = "free_y") +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(
#     x = "",
#     y = ""
#   )

##############

#Speedy average
all_run_average <- fgroup_by(clean_df, time, state, age, vaccination, risk) %>%
  fsummarise(
    q_low = fquantile(value, 0.025),
    q_high = fquantile(value, 0.975),
    value = fmedian(value)
  )


#Plot
ggplot(
  data = subset(all_run_average, age == "All"),
  mapping = aes(
    x = time,
    y = value,
    ymin = q_low,
    ymax = q_high
  )
) +
  geom_line(col = "black") +
  geom_ribbon(alpha = 0.25) +
  theme_bw() +
  labs(x = "",
       y = "") +
  facet_wrap(~state, scale = "free_y")



