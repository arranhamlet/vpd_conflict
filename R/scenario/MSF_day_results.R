options(scipen = 999)

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
  ggpubr,
  patchwork,
  squire.page,
  patchwork,
  data.table
)

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Countries of interest
# countries_interest <- c("PSE, "SDN", "MMR", "PNG", "AFG", "VEN", "HTI", "GTM", "TCD", "DRC", "SOM", "BFA", "GBR")
countries_interest <- c("PSE")

#Load WHO disease data
cases_of_interest <- import("data/processed/WHO/reported_cases_data.csv") %>%
  subset(disease_description %in% c("Pertussis", "Measles", "Diphtheria") & iso3 %in% countries_interest)  %>%
  mutate(disease_description = tolower(disease_description))

#Import model
model <- odin2::odin("models/stochastic_model_v1.R")

#Run through diseases of interest
diseases <- data.frame(
  disease = c("measles", "pertussis", "diphtheria"),
  R0 = c(15, 12, 4)
)

#Loop
double_run <- sapply(countries_interest, function(y){
  
  all_run <- sapply(1:nrow(diseases), function(x){
    print(x)
    
    inside_run <- sapply((diseases$R0[x] - 3):(diseases$R0[x] + 3), function(R0){
      
      tryCatch({
        
        #Run process
        model_data_processed <- data_load_process_wrapper(
          iso = y,
          disease = diseases$disease[x],
          vaccine = diseases$disease[x],
          R0 = R0,
          timestep = "day",
          WHO_seed_switch = T
        )
        
        #Process for plotting
        model_ran <- run_model(
          odin_model = model,
          params = model_data_processed$params,
          time = floor(model_data_processed$time),
          no_runs = 1
        )
        
        deet <- process_for_plotting(model_ran, input_data = model_data_processed$input_data)
        
        export(x = deet[[1]], file = paste0("output/model_run/MSF/", y, "_", diseases$disease[x], "_R0", R0, "_full.csv"))
        export(x = deet[[2]], file = paste0("output/model_run/MSF/", y, "_", diseases$disease[x], "_R0", R0, "_susceptibility.csv"))
        
      })
      
    }, simplify = FALSE)
    
    
  }, simplify = FALSE)
  
})

#Load and process
full <- sapply(list.files("output/model_run/MSF", pattern = "full.csv", full.names = T), function(x){
  
  deets <- unlist(strsplit(last(unlist(strsplit(x, "/"))), "_"))
  df_imported <- import(x) %>%
    mutate(iso = deets[1],
           disease = deets[2],
           R0 = as.numeric(gsub("R0", "", deets[3])))
  
  df_sub <- df_imported %>%
    subset((state %in% c("S", "E", "I", "R", "Is", "Rc") & year == 2023)) %>%
    mutate(state_num = case_when(
      state == "S" ~ 1,
      state != "S" ~ 2
    ))
  
  starting_immunity <- data.frame(
    dim1 = df_sub$age,
    dim2 = df_sub$vaccination,
    dim3 = df_sub$risk,
    dim4 = df_sub$state_num,
    value = df_sub$value,
    iso = deets[1],
    disease = deets[2],
    R0 = gsub("R0", "", deets[3])
  )

  cases <- df_imported %>%
    subset(state == "new_case" & age == "All")
  
  list(starting_immunity,
       cases)
  
}, simplify = FALSE)

full_starting_immunity <- Reduce(rbind, sapply(full, function(x) x[[1]], simplify = FALSE))
full_cases <- Reduce(rbind, sapply(full, function(x) x[[2]], simplify = FALSE))

ggplot(
  data = full_cases %>% 
    subset(disease == "diphtheria" & iso == "PSE" & year > 1960 & year < 1981),
  mapping = aes(
    x = year,
    y = value,
    color = iso
  )
) +
  geom_line() +
  theme_bw() +
  labs(x=  "",
       y = "Annual cases",
       color = "") +
  facet_wrap(R0~disease, scales = "free_y") +
  scale_y_continuous(labels = scales::comma)


susceptibility <- Reduce(rbind, sapply(list.files("output/model_run/MSF", pattern = "susceptibility.csv", full.names = T), function(x){
  
  deets <- unlist(strsplit(last(unlist(strsplit(x, "/"))), "_"))
  
  df_susc <- import(x) %>%
    subset(year == max(year)) %>%
    fgroup_by(age, status) %>%
    fsummarise(value = sum(value)) %>%
    mutate(iso = deets[1],
           disease = deets[2],
           R0 = gsub("R0", "", deets[3])) %>%
    group_by(age) %>%
    mutate(prop = value/sum(value))
  
  
}, simplify = FALSE))

ggplot(
  data = susceptibility %>%
    subset(disease == "measles" & iso %in% "GBR" & R0 == 15 & status == "Susceptible"),
  mapping = aes(
    x = age,
    y = prop,
    fill = status
  )
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "", y = "Prop", fill = "") +
  facet_wrap(R0~iso, ncol = 4)

#Output
export(susceptibility, "output/model_run/MSF/processed/susceptibility.csv")
export(full_cases, "output/model_run/MSF/processed/full_cases.csv")
export(full_starting_immunity, "output/model_run/MSF/processed/full_starting_immunity.csv")

#Prepare params for starting
full_starting_immunity_go <- full_starting_immunity %>%
  mutate(id = paste0(iso, "_", disease, "_", R0)) %>%
  subset(iso != "GTM")
  

total_params <- sapply(unique(full_starting_immunity_go$id), function(x){
  
  print(x)
  this_data <- subset(full_starting_immunity_go, id == x)
  
  data_out <- setup_for_shiny(
    iso = this_data$iso[1],
    disease = this_data$disease[1],
    vaccine = this_data$disease[1],
    R0 = diseases[which(diseases$disease == this_data$disease[1]), ]$R0,
    timestep = "day",
    year_start = 2023,
    year_end = 2023,
    susceptability_distribution = this_data
  )
  
  saveRDS(data_out$params, paste0("output/model_run/MSF/processed/", x, ".rds"))

}, simplify = FALSE)

#Load all again
all_files_loaded <- list.files("output/model_run/MSF/processed/", pattern = ".rds", full.names = T)

all_together <- sapply(all_files_loaded, function(x){
  readRDS(x)
}, simplify = FALSE)

names(all_together) <- gsub(".rds", "", sapply(strsplit(all_files_loaded, "/"), function(x) last(x)))

export(x = all_together, file = "output/model_run/MSF/processed/full_rds.rds")

params <- data_out$params
new_seed <- data_out$params$seeded
new_seed_upd <- abind::abind(new_seed,
                             new_seed,
                             new_seed,
                             along = 4)
new_seed_upd[18, 1, 1, 2] <- 1

params$seeded <- new_seed_upd
params$tt_seeded <- c(0, 10, 11)
params$no_seeded_changes <- 3

#Process for plotting
model_ran <- run_model(
  odin_model = model,
  params = params,
  time = 365,
  no_runs = 10
)

#Quick check
ggplot(
  data = model_ran %>%
    subset(state == "new_case" & age == "All"),
  mapping = aes(
    x = time,
    y = value,
    color = run
  ) 
) +
  geom_line() +
  theme_bw() +
  labs(x = "Time",
       y = "Value") +
  theme(legend.position = "none")


