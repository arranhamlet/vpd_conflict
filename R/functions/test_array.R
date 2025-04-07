
test_array <- function(
    odin_model, 
    dimension_1, 
    dimension_2, 
    dimension_3,
    crude_parameter,
    version = "old"
    ){
  
  #Parameters
  params <- list(
    dimension_1 = dimension_1,
    dimension_2 = dimension_2,
    dimension_3 = dimension_3,
    no_changes = 6,
    tt_changes = 0:5,
    crude_parameter = generate_array_df(
      dim1 = 6,
      dim2 = dimension_1,
      dim3 = dimension_2,
      dim4 = dimension_3,
      updates = crude_parameter
    ) %>%
      df_to_array(version = version)
  )
  
  #' Define the dust system and initialize it with given parameters
  sys <- dust2::dust_system_create(odin_model(), params, n_particles = 1)
  
  #' Set the initial state for the dust system
  dust2::dust_system_set_state_initial(sys)
  
  #' Define the time vector for simulation (starting from 0)
  full_time_vector <- 0:6
  
  #' Run the dust system simulation over the defined time period
  y <- dust2::dust_system_simulate(sys, full_time_vector)
  
  # Run through these and name
  unpacked <- dust_unpack_state(sys, y)
  all_loopered <- do.call(rbind, sapply(1:length(unpacked), function(x){
    
    unpacked_this <- unpacked[[x]]
    dimnames(unpacked_this) <- lapply(dim(unpacked_this), function(y) seq(1, y, by = 1))
    
    reshape2::melt(unpacked_this) %>%
      setNames(c("age", "vaccination", "risk", "time", "value")) %>%
      mutate(state = names(unpacked)[x]) 

  }, simplify = FALSE))
  
  #Aggregate for the all
  full <- all_loopered %>%
    group_by(time, state) %>%
    summarise(value = sum(value)) %>%
    mutate(age = "All",
           vaccination = "All",
           risk = "All")

  list(df = rbind(all_loopered,
                  full) %>%
         mutate(dimension_2 = dimension_2),
       params = params)
  
}