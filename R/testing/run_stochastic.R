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
 
  n_age = 2,
  n_vacc = 1,
  n_risk = 4,
  
  I0 = 0,
  initial_background_death = 1/(.16 * 365),
  aging_rate = 1/(.16 * 365)

)


#Run model
clean_df <- run_model(
  params = params,
  time = 365 * 5,
  no_runs = 5
  )


#Check risk groups
ggplot(
  data = subset(clean_df, (state == "S" | state == "total_pop") & age != "All" & run == "run_1"),
  mapping = aes(
    x = time,
    y = value,
    color = risk,
    linetype = age
  )
) +
  geom_line() +
  facet_wrap(~age, scales = "free_y") +
  theme_bw()

##############


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




# I0 = 0,
# 
# N0 = array(c(0, 1000, 0, 0), dim = c(2, 2, 1)),
# 
# severe_recovery_rate = 1/14,
# prop_severe = 0.1,
# prop_complications = 0.1,
# delta = 0,
# 
# #Seeding parameters
# # seeded = array(c(0, 0, 0, 0,
# #                  0, 10, 0, 0,
# #                  0, 0, 0, 0,
# #                  0, 10, 0, 0,
# #                  0, 0, 0, 0), dim = c(5, 2, 2, 1)),
# # tt_seeded = c(0, 200, 201, 800, 801),
# 
# #Time varying vaccination
# # vaccination_coverage = array(
# #   c(0, 0, 0, 0,
# #     .5/14, .5/14, .5/14, .5/14,
# #     0, 0, 0, 0,
# #     .5/7, .5/7, .5/7, .5/7,
# #     0, 0, 0, 0), dim = c(5, 2, 2, 1)),
# # tt_vaccination_coverage = c(0, 200, 214, 800, 807),
# 
# waning_rate = 0.0001