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

#Create model
model <- odin2::odin({
  
  # Update rule: Add the number of people moved into each compartment
  update(population_one)  <- assigned_values[1]
  update(population_two) <- assigned_values[2]
  
  # Initial state: All compartments start with 0 people
  initial(population_one) <- 0
  initial(population_two) <- 0
  
  # Value
  assigned_values[] <- Multinomial(10, probability)
  
  # Probability
  probability <- parameter()
  
  dim(probability) <- 2
  dim(assigned_values) <- 2
  
})
