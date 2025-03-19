# #Dimensions
# n_age = 1
# n_vacc = 1
# n_vulnerable = 1  
# 
# #Initial values
# N0 = 1000
# I0 = 1
# 
# #Add in contact matrix
# contact_matrix = NULL
# #Disease parameters
# #Incubation rate
# incubation_rate = .2
# #Recovery rate
# recovery_rate = 0.7
# #Recovery rate for severe infections
# severe_recovery_rate = 0
# #Proportion of infections that are severe
# prop_severe = 0
# #Proportion of severe infections that have complications
# prop_complications = 0
# 
# #Vaccination
# vaccination_coverage = NULL
# tt_vaccination_coverage = 0
# age_vaccination_beta_modifier = 1
# 
# #R0
# R0 = 2
# tt_R0 = 0
# 
# #Births deaths and aging
# #Aging rate by compartment
# aging_rate = 0
# #Set initial background death rate
# initial_background_death = 1/(80 * 365)
# #Set simple births and deaths
# simp_birth_death = 1
# #Changing birth rate if  simp_birth_death != 1
# tt_birth_changes = 0
# tt_death_changes = 0
# #Crude birth rate if simp_birth_death != 1
# crude_birth = NULL
# crude_death = NULL
# #Weight given to maternal protection
# protection_weight = 0
# #Age group where maternal protection ends
# age_maternal_protection_ends = 1
# #Reproductive ages
# repro_low = 2
# repro_high = 2



param_packager <- function(

  #Dimensions
  n_age = 1,
  n_vacc = 1,
  n_vulnerable = 1,  
  
  #Initial values
  N0 = 1000,
  I0 = 1,
  
  #Add in contact matrix
  contact_matrix = NULL,
  #Disease parameters
  #Incubation rate
  incubation_rate,
  #Recovery rate
  recovery_rate,
  #Recovery rate for severe infections
  severe_recovery_rate = 0,
  #Proportion of infections that are severe
  prop_severe = 0,
  #Proportion of severe infections that have complications
  prop_complications = 0,

  #Vaccination
  vaccination_coverage = 0,
  tt_vaccination_coverage = 0,
  age_vaccination_beta_modifier = 1,
  
  #R0
  R0,
  tt_R0 = 0,
  
  #Births, deaths and aging
  #Aging rate by compartment
  aging_rate = 0,
  #Set initial background death rate
  initial_background_death = 1/(80 * 365),
  #Set simple births and deaths
  simp_birth_death = 1,
  #Changing birth rate if  simp_birth_death != 1
  tt_birth_changes = 0,
  tt_death_changes = 0,
  #Crude birth rate if simp_birth_death != 1
  crude_birth = 0,
  crude_death = 0,
  #Weight given to maternal protection
  protection_weight = 0,
  #Age group where maternal protection ends
  age_maternal_protection_ends = 1,
  #Reproductive ages
  repro_low = 2,
  repro_high = 2
  
  ){
  

# Calculate additional parameters from inputs -----------------------------

  #Set up contact matrix if now provided
  if(is.null(contact_matrix)){
    contact_matrix <- matrix(1, nrow = n_age, ncol = n_age)/(n_age * n_age)
  }
  
  #Vaccination
  no_vacc_changes <- length(tt_vaccination_coverage)
  
  #R0
  no_R0_changes <- length(tt_R0)
  
  #Format initial population and infections
  N0 <- check_and_format_input(N0, n_age, n_vacc, n_vulnerable)
  I0 <- check_and_format_input(N0, n_age, n_vacc, n_vulnerable)

  #Prop severe
  prop_severe <- check_and_format_input(prop_severe, n_age, n_vacc, n_vulnerable)
  
  #Vaccination
  vaccination_coverage <- check_and_format_input(vaccination_coverage, no_vacc_changes, n_age, n_vacc, n_vulnerable)
  age_vaccination_beta_modifier <- check_and_format_input(age_vaccination_beta_modifier, n_age, n_vacc, n_vulnerable)
  
  #Births, deaths, aging
  initial_background_death <- check_and_format_input(initial_background_death, n_age, n_vulnerable)
  crude_birth <- check_and_format_input(crude_birth, length(tt_birth_changes), n_vulnerable)
  crude_death <- check_and_format_input(crude_death, length(tt_birth_changes), n_age, n_vulnerable)
  no_birth_changes = length(tt_birth_changes)
  no_death_changes = length(tt_death_changes)
  # if(is.null(tt_birth_changes)) tt_birth_changes <- 0
  # if(is.null(tt_death_changes)) tt_death_changes <-0

# Export list -------------------------------------------------------------

  list(
    
    #Dimensions
    n_age = n_age,
    n_vacc = n_vacc,
    n_vulnerable = n_vulnerable,  
    
    #Initial values
    N0 = N0,
    I0 = I0,
    
    #Add in contact matrix
    contact_matrix = contact_matrix,
    
    #Disease parameters
    #Incubation rate
    incubation_rate = incubation_rate,
    #Recovery rate
    recovery_rate = recovery_rate,
    #Recovery rate for severe infections
    severe_recovery_rate = severe_recovery_rate,
    #Prop infections that are severe
    prop_severe = prop_severe,
    #Proportion of severe infections that have complications
    prop_complications = prop_complications,
    
    
    #Vaccination
    vaccination_coverage = vaccination_coverage,
    tt_vaccination_coverage = tt_vaccination_coverage,
    no_vacc_changes = no_vacc_changes,
    age_vaccination_beta_modifier = age_vaccination_beta_modifier,
    
    #R0
    R0 = R0,
    tt_R0 = tt_R0,
    no_R0_changes = no_R0_changes,
    
    #Births, deaths and aging
    #Aging rate by compartment
    aging_rate = aging_rate,
    #Set initial background death rate
    initial_background_death = initial_background_death,
    #Set simple births and deaths
    simp_birth_death = simp_birth_death,
    #Changing birth rate if  simp_birth_death != 1
    tt_birth_changes = tt_birth_changes,
    tt_death_changes = tt_death_changes,
    no_birth_changes = no_birth_changes,
    no_death_changes = no_death_changes,
    #Crude birth rate if simp_birth_death != 1
    crude_birth = crude_birth,
    crude_death = crude_death,
    #Weight given to maternal protection
    protection_weight = protection_weight,
    #Age group where maternal protection ends
    age_maternal_protection_ends = age_maternal_protection_ends,
    #Reproductive ages
    repro_low = repro_low,
    repro_high = repro_high
    
  )

  
}