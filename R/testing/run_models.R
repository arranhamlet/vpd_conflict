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
  initial_population = matrix(100, nrow = 1, ncol = 1),
  birth_rate = 0,
  R0 = 0.99
)

#Define dust system and initialise
sys <- dust2::dust_system_create(model(), pars)
dust2::dust_system_set_state_initial(sys)

#Set time
time <- 0:200
#Run model
y <- dust2::dust_system_simulate(sys, time)

row.names(y) <- c("S", "E", "I", "R")
ygg <- gather(as.data.frame(t(y))) %>%
  mutate(time = rep(time, 4),
         key = factor(key, levels = c("S", "E", "I", "R")))

#Outputs
ggplot(
  data = ygg,
  mapping = aes(
    x = time,
    y = value,
    color = key
  )
) +
  geom_line(lwd = 2) +
  theme_bw() +
  labs(x = "",
       y = "",
       color = "")
