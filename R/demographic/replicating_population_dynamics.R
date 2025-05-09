if(!require("pacman")) install.packages("pacman")

#Load packages
pacman::p_load(
  odin2,
  rio,
  here,
  dust2,
  tidyverse,
  reshape2,
  collapse,
  janitor,
  patchwork,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Load files needed
migration <- import(here("data", "processed", "WPP", "migration.csv"))
fertility <- import(here("data", "processed", "WPP", "fertility.csv"))
mortality <- import(here("data", "processed", "WPP", "deaths.csv"))
population_all <- import(here("data", "processed", "WPP", "age_both.csv"))
population_female <- import(here("data", "processed", "WPP", "age_female.csv"))
contact_matricies <- import(here("data", "raw", "contact_matricies", "contact_all.rdata"))

random_iso <- c(sample(unique(migration$iso3), 7), "GBR")

all_plot_together <- sapply(random_iso, function(x){
  
  #Run function - add in modifiers to account for Gaza
  demog_data <- process_demography(
    migration = migration, 
    fertility = fertility, 
    mortality = mortality, 
    population_all = population_all, 
    population_female = population_female,
    year_start = "",
    year_end = "",
    iso = x,
    n_age = 101,
    number_of_vaccines = 2,
    contact_matricies = contact_matricies
  )

  #Set up model
  params <- param_packager(
    
    n_age = demog_data$input_data$n_age,
    n_vacc = demog_data$input_data$n_vacc,
    n_risk = demog_data$input_data$n_risk,
    
    N0 = demog_data$N0,
    
    contact_matrix = demog_data$contact_matrix,
    I0 = 0,
    initial_background_death = 1,
    aging_rate = 1,
    
    #Changing mortality and birth
    #Turn off simple birth/deaths
    simp_birth_death = 0,
    #List of when birth_death_changes
    tt_birth_changes = demog_data$tt_migration,
    tt_death_changes = demog_data$tt_migration,
    #Values of changes
    crude_birth = demog_data$crude_birth,
    crude_death = demog_data$crude_death,
    #Birth ages
    repro_low = 15,
    repro_high = 49,
    
    #Migration
    tt_migration = demog_data$tt_migration,
    migration_in_number = demog_data$migration_in_number,
    migration_distribution_values = demog_data$migration_distribution_values,
    migration_represent_current_pop = 1
    
  )
  
  #Run model
  clean_df <- run_model(
    odin_model = model,
    params = params,
    time = length(demog_data$tt_migration),
    no_runs = 1
  )
  
  #Generate plots
  saved_plots <- model_run_demographic_plots(
    clean_model_df = clean_df,
    demog_data,
    end_year = 2022
  )
  
  upd1 <- saved_plots[[1]] + 
    labs(title = unique(migration[which(migration$iso3 == x), ]$area), subtitle = "") +
    theme(legend.position = "none")
  
  upd2 <- saved_plots[[3]] + 
    labs(title = "", 
         subtitle = "",
         y = "Prop population") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90))
  
  plot <- upd1 + upd2 + 
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
    
  ggsave(paste0("figs/demography/", x, "_population.jpg"), 
         plot,
         width = 8, height = 4)
  
}, simplify = FALSE)






