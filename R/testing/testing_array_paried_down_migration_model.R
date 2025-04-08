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
loop_this <- sapply(1:10, function(n_vacc){
  
  print(n_vacc) # Print progress indicator
  n_vacc_comp <- n_vacc
  
  n_age <- 3
  n_risk <- 1
  
  tt_migration <- round(seq(0, 24, length.out = 9))
  
  N0 <- array(0, dim = c(n_age, n_vacc, n_risk))
  N0[1, 1, 1] <- 1000
  
  migration_in_number <- array(0, dim = c(length(tt_migration), n_age, n_vacc, n_risk))
  migration_in_number[, 1, 1, 1] <- c(0, 100, 200, 
                                       -500, 300, 100,
                                       1000, -700, -300)
  
  migration_distribution_values <- array(0, dim = c(length(tt_migration), 6, n_age, n_vacc, n_risk))
  migration_distribution_values[1, 1, 1, 1, 1] <- 1
  
  # Define model parameters
  params <- list(
    #Set up demographics
    n_age = n_age,            
    n_vacc = n_vacc,        
    n_risk = 1,             
    N0 = N0,
    
    #Migration
    no_migration_changes = length(tt_migration), 
    tt_migration = tt_migration,      
    migration_in_number = migration_in_number,
    migration_distribution_values = migration_distribution_values
  )
  
  #' Define the dust system and initialize it with given parameters
  sys <- dust2::dust_system_create(model(), params, n_particles = 1)
  
  #' Set the initial state for the dust system
  dust2::dust_system_set_state_initial(sys)
  
  #' Define the time vector for simulation (starting from 0)
  full_time_vector <- 0:23
  
  #' Run the dust system simulation over the defined time period
  y <- dust2::dust_system_simulate(sys, full_time_vector)
  
  #Unpacked everything
  all_unpacked <- dust_unpack_state(sys, y)
  
  #Standard shaped compartments
  standard_comp <- c("S", "E", "I", "R", "Is", "Rc", "output_migration_interpolate")
  vector_comp <- c("total_pop", "migration_leftover")
  
  #Standard unpacked
  standard_unpacked <- do.call(rbind, sapply(standard_comp, function(state){
    
    this_state <- all_unpacked[[state]] %>%
      reshape2::melt() %>%
      setnames(c("n_age", "n_vacc", "n_risk", "time", "value")) %>%
      mutate(state = state)
    
      rbind(this_state, this_state %>%
              group_by(state, time) %>%
              summarise(value = sum(value), .groups = "drop") %>%
              mutate(n_age = "All",
                     n_vacc = "All",
                     n_risk = "All"))
    
  }, simplify = FALSE))
  
  #Vector unpacked
  vector_unpacked <- do.call(rbind, sapply(vector_comp, function(vector){
    
    this_vector <- all_unpacked[[vector]] 
    
    data.frame(n_age = "All",
               n_vacc = "All",
               n_risk = "All",
               time = 1:length(this_vector),
               value = this_vector,
               state = vector)
    
  }, simplify = FALSE))
  
  input_migration_unpacked <- all_unpacked$input_migration_interpolate %>%
    reshape2::melt() %>%
    setnames(c("event", "n_age", "n_vacc", "n_risk", "time", "value")) %>%
    group_by(n_age, n_vacc, n_risk, time) %>%
    summarise(value = sum(as.numeric(value)), .groups = "drop") %>%
    mutate(state = "input_migration_interpolate")
    
    clean_df <- rbind(
      standard_unpacked,
      vector_unpacked,
      rbind(input_migration_unpacked,
            input_migration_unpacked  %>%
              group_by(state, time) %>%
              summarise(value = sum(value), .groups = "drop") %>%
              mutate(n_age = "All",
                     n_vacc = "All",
                     n_risk = "All"))) %>% 
      mutate(n_vacc_comp = n_vacc_comp,
             state = factor(state, levels = c(standard_comp, vector_comp, "input_migration_interpolate")))
  
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
  data = subset(all_looped, n_age == "All"),
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
  scale_y_continuous(label = scales::comma) +
  facet_wrap(~state, scales = "free_y")

#Okay check everything goin in. Have to aggregate because it has the extra dimension of time at the start
#Looks good
ggplot(
  data = subset(all_looped, n_age != "All" & state == "input_migration_interpolate")  %>%
    group_by(time, state, n_vacc, n_vacc_comp) %>%
    summarise(value = sum(value)),
  mapping = aes(
    x = time,
    y = value,
    color = as.factor(n_vacc) # Color by number of vaccination compartments
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
  data = subset(all_looped, state == "output_migration_interpolate" & n_age == "All"),
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
    title = "Population outflow at different numbers of vaccination compartments"
  ) +
  scale_y_continuous(label = scales::comma)


# Plot 2: Snapshot across age groups for susceptible population (non-aggregated by age)
# Looks good - all the age profiles are the same, and only those in compartment 1 are there
ggplot(
  data = subset(all_looped, state == "S" & time == 1 & n_age != "All"),
  mapping = aes(
    x = as.factor(n_age),
    y = value,
    fill = as.factor(n_vacc) # Color by vaccination compartment
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

ggplot(
  data = subset(all_looped, state == "S" & time == 5 & n_age != "All"),
  mapping = aes(
    x = as.factor(n_vacc),
    y = value,
    fill = as.factor(n_vacc) # Color by vaccination compartment
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
    x = "Vaccination",
    y = "Population",
    title = "Population by age at time 5 for different numbers of vaccination compartments"
  ) +
  scale_y_continuous(label = scales::comma)

#Aggregate down across vaccination and compartments
#Okay, only vaccination 1 and S have values, all looks good
all_looped %>%
  group_by(state, n_vacc, n_vacc_comp) %>%
  summarise(value = sum(value)) %>%
  filter(n_vacc != "All" & value != 0)

#Okay, are the parameters for migration_in_number the same?
#For all columns that are NOT the first vaccination compartment?
sum(all_params[[1]]$migration_in_number[, , -1, ])
sum(all_params[[2]]$migration_in_number[, , -1, ])
sum(all_params[[3]]$migration_in_number[, , -1, ])
#For all the column that IS the first vaccination compartment
sum(all_params[[1]]$migration_in_number[, , 1, ])
sum(all_params[[2]]$migration_in_number[, , 1, ])
sum(all_params[[3]]$migration_in_number[, , 1, ])





