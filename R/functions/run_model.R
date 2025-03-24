
run_model <- function(params, time = 1000, no_runs = 25){
  
  #Define dust system and initialise
  sys <- dust2::dust_system_create(model(), params, n_particles = no_runs)
  dust2::dust_system_set_state_initial(sys)
  
  #Set time and run model
  time <- 0:time
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
  
  
  
  
}