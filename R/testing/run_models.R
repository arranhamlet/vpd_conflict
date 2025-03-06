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
  init_inf = matrix(1, nrow = 1, ncol = 1),
  # birth_rate = 0,
  R0 = 0.9,
  birth_rate = 0,
  death_rate = 0
)

#Define dust system and initialise
sys <- dust2::dust_system_create(model(), pars)
dust2::dust_system_set_state_initial(sys)

#Set time
time <- 0:500
#Run model
y <- dust2::dust_system_simulate(sys, time)

row.names(y) <- c("S", "E", "I", "R", "N", "foi")
ygg <- gather(as.data.frame(t(y))) %>%
  mutate(time = rep(time, length(row.names(y))),
         key = factor(key, levels = c("S", "E", "I", "R", "N", "foi")))

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
       color = "") +
  facet_wrap(~key, scales = "free")


foi_vec <- rep(0:99)
I_vec <- rep(0:99)
S <- 100
I <- 1

for(i in 0:100){
  
  beta = 0.0872449
  foi <- beta * S * (I / 100)
  
  I <- I + foi
  foi_vec[i] <- foi
  I_vec[i] <- I
  
  S <- S - foi
  
}











