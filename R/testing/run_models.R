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
  R0 = 1.01,
  N0 = matrix(1000, nrow = 1, ncol = 1),
  I0 = matrix(1, nrow = 1, ncol = 1),
  recovery_rate = 1/14,
  incubation_rate = 1/5,
  b = 0,#1/(8 * 365),
  severe_recovery_rate = 1/14,
  prop_severe = matrix(.25, nrow = 1, ncol = 1),
  prop_complications = 0.1,
  age_vaccination_beta_modifier = matrix(1, nrow = 1, ncol = 1)
)

#Define dust system and initialise
sys <- dust2::dust_system_create(model(), pars)
dust2::dust_system_set_state_initial(sys)

#Set time
time <- 0:1000
#Run model
y <- dust2::dust_system_simulate(sys, time)
state <- dust_unpack_state(sys, y)

output <- matrix(
  unlist(state), ncol = length(state), byrow = F
  ) %>%
  as.data.frame() %>%
  set_names(
    names(state)
    ) %>%
  rownames_to_column(
    var = "time"
    ) %>%
  mutate(
    time = as.numeric(time)
  ) %>%
  gather(
    key = "key",
         value = "value",
         -time
    ) %>%
  mutate(
    key = factor(key, levels = names(state))
  )


#Plot
ggplot(
  data = output,
  mapping = aes(
    x = time,
    y = value
  )
) +
  geom_point() +
  facet_wrap(
    ~key,
    scales = "free_y"
  )






