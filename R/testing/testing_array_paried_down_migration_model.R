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

# Load demographic input files (WPP = UN World Population Prospects) for Palestine
migration <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_gen_f01_demographic_indicators_compact.csv"))
fertility <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_fert_f01_fertility_rates_by_single_age_of_mother.csv"))
mortality <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_mort_f01_1_deaths_single_age_both_sexes.csv"))
population_all <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_pop_f01_1_population_single_age_both_sexes.csv"))
population_female <- import(here("data", "raw_data", "WPP_data", "stateofpalestine__wpp2024_pop_f01_3_population_single_age_female.csv"))

# Prepare demographic data for modeling (includes population, migration, etc.)
# Likely adjusts for Gaza context and prepares arrays for model input
demog_data <- prepare_demographic_for_model(
  migration = migration, 
  fertility = fertility, 
  mortality = mortality, 
  population_all = population_all, 
  population_female = population_female,
  year_start = 2000,
  year_end = "" # Possibly uses full range available
)

# Run simulations across different numbers of vaccination compartments: 1, 3, 5
loop_this <- sapply(c(1, 3, 5), function(n_vacc){
  
  print(n_vacc) # Print progress indicator
  
  # Define model parameters
  params <- list(
    
    n_age = 101,            # Age groups (e.g., 0-100)
    n_vacc = n_vacc,        # Number of vaccination compartments
    n_risk = 1,             # Only one risk group used here
    
    N0 = generate_array_df( # Initial population
      dim1 = 101,
      dim2 = n_vacc,
      dim3 = 1,
      updates = demog_data$N0
    ) %>% df_to_array,      # Convert to array format for the model
    
    #Migration
    no_migration_changes = 24, # Number of time points with migration data
    tt_migration = 0:23,       # Time points for migration
    
    migration_in_number = generate_array_df( # Absolute number of migrants
      dim1 = 24,
      dim2 = 101,
      dim3 = n_vacc,
      dim4 = 1,
      updates = demog_data$migration_in_number
    ) %>% df_to_array,
    
    migration_distribution_values = generate_array_df( # How migrants are distributed
      dim1 = 24,
      dim2 = 6,     # Number of compartments (S, E, I, R, Is, Rc)
      dim3 = 101,
      dim4 = n_vacc,
      dim5 = 1,
      updates = demog_data$migration_distribution_values
    ) %>% df_to_array
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

# Plot 2: Snapshot across age groups for susceptible population (non-aggregated by age)
# Looks good - all the age profiles are the same, and only those in compartment 1 are there
ggplot(
  data = subset(all_looped, run == "run_1" & state == "S" & age != "All"),
  mapping = aes(
    x = as.numeric(age),
    y = value,
    color = as.factor(vaccination) # Color by vaccination compartment
  )
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  facet_wrap(~n_vacc_comp) +
  labs(
    x = "Age",
    y = "Population",
    title = "Population by age over time for different numbers of vaccination compartments"
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









