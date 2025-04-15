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
  monty         # Modeling tools
)

# ------------------------------------------------------------------------------
# ODIN2 MODEL DEFINITION: Simulates movement of people into a compartment
# ------------------------------------------------------------------------------

model <- odin2::odin({
  
  # Update rule: Add the number of people moved into each compartment
  update(pop_here[, , ])  <- population_movement[i, j, k]

  # Initial state: All compartments start with 0 people
  initial(pop_here[, , ]) <- 0

  # Interpolate 
  population_movement <- interpolate(time_movement, number_of_people_moving, mode = "constant")

  # Parameters defining the dimensions of the simulation
  dimension_one <- parameter()
  dimension_two <- parameter()
  dimension_three <- parameter()
  
  # Movement event configuration
  time_of_event <- parameter()
  time_movement <- parameter()
  number_of_people_moving <- parameter()

  # Define dimensions of each array
  dim(pop_here) <- c(dimension_one, dimension_two, dimension_three)
  dim(population_movement) <- c(dimension_one, dimension_two, dimension_three)
  dim(number_of_people_moving) <- c(time_of_event, dimension_one, dimension_two, dimension_three)
  dim(time_movement) <- time_of_event
  
})

# ------------------------------------------------------------------------------
# TESTING THE MODEL WITH DIFFERENT DIMENSION SIZES AND MOVEMENT PATTERNS
# ------------------------------------------------------------------------------

# Define time points when movement occurs and the corresponding populations moving
time_movement = c(0, 10, 15, 25)
population_that_moves <- 1

# Total number of movement events
time_of_event <- length(time_movement)

# Loop through different sizes of `dimension_two` (e.g. 1, 3, 5)
loop_dim_size <- sapply(c(1, 3, 5), function(x){
  
  #Assign values to dimensions
  dimension_one <- 1
  dimension_three <- 1
  
  # Initialize array for number of people moving (only assign values to the location [, 1, 1, 1]), regardless of the number of dimensions
  number_of_people_moving <- array(0, dim = c(time_of_event, dimension_one, x, dimension_three))
  number_of_people_moving[, 1, 1, 1] <- population_that_moves
  
  # Bundle parameters
  params <- list(
    dimension_one = dimension_one,
    dimension_two = x,
    dimension_three = dimension_three,
    time_movement = time_movement,
    time_of_event = time_of_event,
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
  this_state <- all_unpacked[[1]] %>%
      reshape2::melt() %>%
      setnames(c("dim1", "dim2", "dim3", "time", "value"))
    
    # Add totals over all dimensions
   df_full <- rbind(this_state, this_state %>%
            group_by(time) %>%
            summarise(value = sum(value), .groups = "drop") %>%
            mutate(dim1 = "All", dim2 = "All", dim3 = "All")) %>%
    mutate(dimension_two_length = x)
  
  list(df_full, params)
  
}, simplify = FALSE)

# Combine results for all dimension sizes into a single data frame
full_df <- do.call(rbind, sapply(loop_dim_size, function(x) x[[1]], simplify = FALSE))

# ------------------------------------------------------------------------------
# PLOT RESULTS
# ------------------------------------------------------------------------------

# Plot the total values (dim1 == "All") across time, coloured by dimension size
# I would assume these would all be the same, but they are not
ggplot(data = subset(full_df, dim1 == "All"),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(dimension_two_length)
       )) +
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Value",
       color = "Dimension 2 length")

#I would have assumed to only see one line - the 1st value of dim2 given how I assigned values in my array - but they are found in other vaccine compartments 
ggplot(data = subset(full_df, dim1 != "All"),
       mapping = aes(
         x = time,
         y = value,
         color = as.factor(dim2)
       )) +
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Value",
       color = "dim 2 value") +
  facet_wrap(dim1~dimension_two_length, ncol = 3)


