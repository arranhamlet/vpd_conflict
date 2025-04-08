# Install 'pacman' package if not already installed
if(!require("pacman")) install.packages("pacman")

# Prevent scientific notation in outputs (e.g., 1e+05 becomes 100000)
options(scipen = 999)

# Load required packages using pacman (installs if missing)
pacman::p_load(
  odin2,        # Model definition language for compartmental models
  rio,          # Simplifies file import/export
  here,         # Simplifies paths relative to project root
  dust2,        # For running stochastic simulations with odin2 models
  tidyverse,    # Collection of data manipulation and visualization packages
  reshape2,     # For reshaping data (e.g., melt, dcast)
  collapse,     # Fast data transformation tools
  janitor,      # Data cleaning tools (e.g., clean column names)
  monty,        # Custom or niche modeling tools (likely internal use)
  data.table    # High-performance data frame operations
)

# Source all R scripts from the 'R/functions' directory (custom functions)
invisible(sapply(
  list.files("R/functions", full.names = TRUE, pattern = ".R", recursive = FALSE),
  function(x) source(x)
))

# Import odin2 model from file - a simplified model focusing only on migration
model <- odin2::odin("models/stochastic_migration_testing.R")

# Run simulations across different numbers of vaccination compartments: 1, 3, 5
loop_this <- sapply(c(1, 3, 5), function(n_vacc){
  
  print(n_vacc) # Print progress indicator
  
  n_age <- 1
  n_risk <- 1
  
  tt_migration <- 0:23 
  N0 <- array(100, dim = c(n_age, n_vacc, n_risk))
  migration_in_number <- array(0, dim = c(length(tt_migration), n_age, n_vacc, n_risk))
  migration_in_number[, , 1, 1] <- 100
  
  migration_distribution_values <- array(0, dim = c(length(tt_migration), 6, n_age, n_vacc, n_risk))
  migration_distribution_values[, 1, , 1, 1] <- 1
  
  # Define model parameters
  params <- list(
    
    #Set up demographics
    n_age = n_age,            # Age groups (e.g., 0-100)
    n_vacc = n_vacc,        # Number of vaccination compartments
    n_risk = 1,             # Only one risk group used here
    N0 = N0,
    
    #Migration
    no_migration_changes = length(tt_migration), # Number of time points with migration data
    tt_migration = tt_migration,       # Time points for migration
    migration_in_number = migration_in_number,
    migration_distribution_values = migration_distribution_values
  )
  
  # Run the model with defined parameters and for time steps equal to migration data
  clean_df <- run_model(
    params = params,
    time = length(demog_data$tt_migration),
    no_runs = 2  # Number of stochastic simulations
  ) %>%
    mutate(n_vacc_comp = n_vacc) # Add identifier for vaccination stratification
  
  # Return model output and parameters for later use
  list(clean_df, params)
  
}, simplify = FALSE) # Output is a list (not simplified to matrix)

# Combine all simulation results (from each n_vacc run) into one dataframe
all_looped <- data.table::rbindlist(
  sapply(loop_this, function(x) x[[1]], simplify = FALSE)
)

# Extract corresponding parameter sets
all_params <- sapply(loop_this, function(x) x[[2]], simplify = FALSE)

# === Visualization ===

# Plot 1: Time series of susceptible individuals across vaccination schemes (total population)
#Boooo, populations are different
ggplot(
  data = subset(all_looped, run == "run_1" & state == "S" & age == "All"),
  mapping = aes(
    x = time,
    y = value,
    color = as.factor(n_vacc_comp) # Color by number of vaccination compartments
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = "" 
  ) +
  labs(
    x = "Time",
    y = "Population",
    title = "Population over time for different numbers of vaccination compartments"
  ) +
  scale_y_continuous(label = scales::comma)

#Okay check everything goin in. Have to aggregate because it has the extra dimension of time at the start
#Looks good
ggplot(
  data = subset(all_looped, run == "run_1" & age != "All" & state == "input_migration_interpolate")  %>%
    group_by(time, state, vaccination, n_vacc_comp) %>%
    summarise(value = sum(value)),
  mapping = aes(
    x = time,
    y = value,
    color = as.factor(vaccination) # Color by number of vaccination compartments
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = "" 
  ) +
  labs(
    x = "Time",
    y = "Population",
    title = "Population over time for different numbers of vaccination compartments"
  ) +
  scale_y_continuous(label = scales::comma) +
  facet_wrap(~n_vacc_comp)

#Now check on it post integration.
#Boo
ggplot(
  data = subset(all_looped, run == "run_1" & state == "output_migration_interpolate" & age == "All"),
  mapping = aes(
    x = time,
    y = value,
    color = as.factor(n_vacc_comp) # Color by number of vaccination compartments
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = "" 
  ) +
  labs(
    x = "Time",
    y = "Population",
    title = "Population over time for different numbers of vaccination compartments"
  ) +
  scale_y_continuous(label = scales::comma)


# Plot 2: Snapshot across age groups for susceptible population (non-aggregated by age)
# Looks good - all the age profiles are the same, and only those in compartment 1 are there
ggplot(
  data = subset(all_looped, run == "run_1" & state == "S" & time == 5 & age != "All"),
  mapping = aes(
    x = as.factor(age),
    y = value,
    fill = as.factor(vaccination) # Color by vaccination compartment
  )
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  facet_wrap(~n_vacc_comp) +
  labs(
    x = "Age",
    y = "Population",
    title = "Population by age at time 5 for different numbers of vaccination compartments"
  ) +
  scale_y_continuous(label = scales::comma)

#Aggregate down across vaccination and compartments
#Okay, only vaccination 1 and S have values, all looks good
all_looped %>%
  group_by(state, vaccination, run, n_vacc_comp) %>%
  summarise(value = sum(value)) %>%
  filter(vaccination != "All" & run == "run_1" & value != 0)

#But look, theyre here!!!
ggplot(
  data = subset(all_looped, run == "run_1" & state == "migration_leftover" & age == "All"),
  mapping = aes(
    x = time,
    y = value,
    color = as.factor(n_vacc_comp) # Color by number of vaccination compartments
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = "" 
  ) +
  labs(
    x = "Time",
    y = "Population",
    title = "Population leftover from migration compartments"
  ) +
  scale_y_continuous(label = scales::comma) 



#Okay, are the parameters for migration_in_number the same?
#For all columns that are NOT the first vaccination compartment?
sum(all_params[[1]]$migration_in_number[, , -1, ])
sum(all_params[[2]]$migration_in_number[, , -1, ])
sum(all_params[[3]]$migration_in_number[, , -1, ])
#For all the column that IS the first vaccination compartment
sum(all_params[[1]]$migration_in_number[, , 1, ])
sum(all_params[[2]]$migration_in_number[, , 1, ])
sum(all_params[[3]]$migration_in_number[, , 1, ])





