# Install 'pacman' package if not already installed
if(!require("pacman")) install.packages("pacman")

# Prevent scientific notation in outputs (e.g., 1e+05 becomes 100000)
options(scipen = 999)

# Load required packages using pacman (installs if missing)
pacman::p_load(
  dust2,        # For running stochastic simulations with odin2 models
  tidyverse,    # Collection of data manipulation and visualization packages
  reshape2,     # For reshaping data (e.g., melt, dcast)
  odin2,        # Model definition language for compartmental models
  data.table,   # For data manipulation
  monty         # Modeling tools (likely internal use)
)

# ------------------------------------------------------------------------------
# ODIN2 MODEL DEFINITION: Simulates movement of people into two compartments
# ------------------------------------------------------------------------------

model <- odin2::odin({
  
  # Update rule: Add the number of people moved into each compartment
  update(pop_here[, , ])  <- pop_here[i, j, k] + movement_here[i, j, k] * movement_direction[i, j, k]
  update(pop_there[, , ]) <- pop_there[i, j, k] + movement_there[i, j, k] * movement_direction[i, j, k]
  
  # Initial state: All compartments start with 0 people
  initial(pop_here[, , ]) <- 0
  initial(pop_there[, , ]) <- 0
  
  # Interpolate movement over time: number of people moving
  population_movement <- interpolate(time_movement, number_of_people_moving, mode = "constant")

  #Change symbol of population_movement to allow for negative values later on
  movement_direction[, , ] <- if(population_movement[i, j, k] < 0) -1 else 1
  population_movement_updated[, , ] <- population_movement[i, j, k] * movement_direction[i, j, k]
  
  #Movement
  movement_here[, , ] <- Binomial(population_movement_updated[i, j, k], 0.7)
  movement_there[, , ] <- Binomial(population_movement_updated[i, j, k], 0.3)
  
  # Parameters defining the dimensions of the simulation
  dimension_one <- parameter()
  dimension_two <- parameter()
  dimension_three <- parameter()
  
  # Movement event configuration
  number_of_movement_events <- parameter()
  time_movement <- parameter()
  number_of_people_moving <- parameter()

  # Define dimensions of each array
  dim(pop_here) <- c(dimension_one, dimension_two, dimension_three)
  dim(pop_there) <- c(dimension_one, dimension_two, dimension_three)
  dim(movement_here) <- c(dimension_one, dimension_two, dimension_three)
  dim(movement_there) <- c(dimension_one, dimension_two, dimension_three)
  dim(population_movement_updated) <- c(dimension_one, dimension_two, dimension_three)
  dim(movement_direction) <- c(dimension_one, dimension_two, dimension_three)
  dim(population_movement) <- c(dimension_one, dimension_two, dimension_three)
  dim(number_of_people_moving) <- c(number_of_movement_events, dimension_one, dimension_two, dimension_three)
  dim(time_movement) <- number_of_movement_events
  
})

# ------------------------------------------------------------------------------
# TESTING THE MODEL WITH DIFFERENT DIMENSION SIZES AND MOVEMENT PATTERNS
# ------------------------------------------------------------------------------

# Define time points when movement occurs and the corresponding populations moving
time_movement = c(0, 10, 12, 13, 20, 30)
population_that_moves <- c(100, 300, 500, 700, 200, -1000)

# Total number of movement events
number_of_movement_events <- length(time_movement)

# Loop through different sizes of `dimension_two` (e.g. 1, 3, 5)
loop_dim_size <- sapply(c(1, 3, 5), function(x){
  
  #Assign values to dimensions
  dimension_one <- 1
  dimension_three <- 1
  
  # Initialize array for number of people moving (only from [1,1,1] cell)
  number_of_people_moving <- array(0, dim = c(number_of_movement_events, dimension_one, x, dimension_three))
  number_of_people_moving[, 1, 1, 1] <- population_that_moves
  
  # Bundle parameters
  params <- list(
    dimension_one = dimension_one,
    dimension_two = x,
    dimension_three = dimension_three,
    time_movement = time_movement,
    number_of_movement_events = number_of_movement_events,
    number_of_people_moving = number_of_people_moving
  )
  
  # Create Dust system and simulate
  sys <- dust2::dust_system_create(model(), params, n_particles = 1)
  dust2::dust_system_set_state_initial(sys)
  full_time_vector <- 0:(max(time_movement) + 5)
  y <- dust2::dust_system_simulate(sys, full_time_vector)
  
  # Unpack simulation output
  all_unpacked <- dust_unpack_state(sys, y)
  
  # Reformat simulation results into a long data frame for plotting
  df_full <- do.call(rbind, sapply(names(all_unpacked), function(state){
    
    this_state <- all_unpacked[[state]] %>%
      reshape2::melt() %>%
      setnames(c("dim1", "dim2", "dim3", "time", "value")) %>%
      mutate(state = state)
    
    # Add totals over all dimensions
    rbind(this_state, this_state %>%
            group_by(state, time) %>%
            summarise(value = sum(value), .groups = "drop") %>%
            mutate(dim1 = "All", dim2 = "All", dim3 = "All"))
    
  }, simplify = FALSE)) %>%
    mutate(dimension_two_length = x)
  
  list(df_full, params)
  
}, simplify = FALSE)

# Combine results for all dimension sizes into a single data frame
full_df <- do.call(rbind, sapply(loop_dim_size, function(x) x[[1]], simplify = FALSE))

# ------------------------------------------------------------------------------
# PLOT RESULTS
# ------------------------------------------------------------------------------

# Plot the total values (dim1 == "All") across time, coloured by dimension size
ggplot(data = subset(full_df, dim1 == "All"),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(dimension_two_length)
       )) +
  geom_line() +
  theme_bw() +
  labs(x = "",
       y = "",
       color = "Dimension 2 length") +
  facet_wrap(~state)

ggplot(data = subset(full_df, dim1 != "All" & state == "pop_here"),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(dim2)
       )) +
  geom_line() +
  theme_bw() +
  labs(x = "",
       y = "",
       color = "Dimension 2 length") +
  facet_wrap(dim1~dimension_two_length, ncol = 3)















