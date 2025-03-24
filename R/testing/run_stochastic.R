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
time1 <- Sys.time()

params <- param_packager(
 
  n_age = 2,
  n_vacc = 2,
  n_risk = 1,
  
  incubation_rate = 0.2,
  recovery_rate = 1/14,
  R0 = 10,
  initial_background_death = 0.001,
  aging_rate = 0.005,
  I0 = 0,
  
  N0 = array(c(0, 1000, 0, 0), dim = c(2, 2, 1)),
  
  severe_recovery_rate = 1/14,
  prop_severe = 0.1,
  prop_complications = 0.1,
  delta = 0.001,
  
  #Seeding parameters
  seeded = array(c(0, 0, 0, 0,
                   0, 10, 0, 0,
                   0, 0, 0, 0,
                   0, 10, 0, 0,
                   0, 0, 0, 0), dim = c(5, 2, 2, 1)),
  tt_seeded = c(0, 200, 201, 800, 801),
  
  #Time varying vaccination
  vaccination_coverage = array(
    c(0, 0, 0, 0,
      .5/14, .5/14, .5/14, .5/14,
      0, 0, 0, 0,
      .5/7, .5/7, .5/7, .5/7,
      0, 0, 0, 0), dim = c(5, 2, 2, 1)),
  tt_vaccination_coverage = c(0, 200, 214, 800, 807),
  
  waning_rate = 0.0001

)

#Run model
clean_df <- run_model(
  params = params,
  time = 365 * 5,
  no_runs = 100
  )

time2 <- Sys.time()

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

#All vaccination coverage
no_runs <- length(unique(clean_df$run))

all_run_vacc <- fgroup_by(subset(clean_df, age != "All"), time, vaccination) %>%
  fsummarise(
    value = sum(value)/no_runs
  ) %>%
  spread(
    key = vaccination,
    value = value
  ) %>%
  set_names("time", "unvaccinated", "vaccinated") %>%
  mutate(
    vac_coverage = vaccinated/(unvaccinated + vaccinated)
  )

#Plot vaccination coverage
ggplot(
  data = all_run_vacc,
  mapping = aes(
    x = time,
    y = vac_coverage
  )
) +
  geom_line() +
  labs(
    x = "",
    y = "Vaccination coverage"
  ) +
  theme_bw()


