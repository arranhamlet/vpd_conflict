#' Parameter Packager Function
#' 
#' This function packages parameters for an infectious disease model with multiple compartments, vaccination, and demographic processes.
#' 
#' @param n_age Number of age groups
#' @param n_vacc Number of vaccination statuses
#' @param n_risk Number of risk groups
#' @param N0 Initial population size
#' @param I0 Initial number of infections
#' @param contact_matrix Contact matrix for transmission rates
#' @param incubation_rate Rate of incubation period progression
#' @param recovery_rate Rate of recovery for non-severe cases
#' @param severe_recovery_rate Rate of recovery for severe cases
#' @param prop_severe Proportion of infections that are severe
#' @param prop_complications Proportion of severe cases with complications
#' @param delta Rate of natural protection waning
#' @param vaccination_coverage Proportion vaccinated
#' @param tt_vaccination_coverage Time points for changes in vaccination coverage
#' @param age_vaccination_beta_modifier Modifier for vaccination impact by age
#' @param waning_rate Rate of immunity waning
#' @param R0 Basic reproduction number
#' @param tt_R0 Time points for changes in R0
#' @param seeded Initial seeding of infections
#' @param tt_seeded Time points for changes in seeding
#' @param aging_rate Rate at which individuals move between age groups
#' @param initial_background_death Baseline background death rate
#' @param simp_birth_death Toggle for simplified births and deaths
#' @param tt_birth_changes Time points for changes in birth rate
#' @param tt_death_changes Time points for changes in death rate
#' @param crude_birth Crude birth rate
#' @param crude_death Crude death rate
#' @param protection_weight_vacc Weight for maternal protection from vaccination
#' @param protection_weight_rec Weight for maternal protection from natural recovery
#' @param age_maternal_protection_ends Age at which maternal protection ends
#' @param repro_low Minimum age for reproduction
#' @param repro_high Maximum age for reproduction
#' 
#' @return A list of formatted parameters ready for use in the infectious disease model
#' @export
param_packager <- function(
    
  #Dimensions
  n_age = 1,
  n_vacc = 1,
  n_risk = 1,  
  
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
  severe_recovery_rate = NULL,
  #Proportion of infections that are severe
  prop_severe = 0,
  #Proportion of severe infections that have complications
  prop_complications = 0,
  #Natural protection waning
  delta = 0,
  
  #Vaccination
  vaccination_coverage = 0,
  tt_vaccination_coverage = 0,
  age_vaccination_beta_modifier = 1,
  waning_rate = 0,
  
  #R0
  R0,
  tt_R0 = 0,
  
  #Seeding parameters
  seeded = 0,
  #Define times when seeding occurs
  tt_seeded = 0,

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
  protection_weight_vacc = 0,
  protection_weight_rec = 0,
  
  #Age group where maternal protection ends
  age_maternal_protection_ends = 1,
  #Reproductive ages
  repro_low = 1,
  repro_high = NULL
  
){
  
  # Calculate additional parameters from inputs -----------------------------
  
  #Set up contact matrix if now provided
  if(is.null(contact_matrix)){
    contact_matrix <- matrix(1, nrow = n_age, ncol = n_age)/(n_age * n_age)
  }
  
  #Set up severe recovery rate if missing
  if(is.null(severe_recovery_rate)) severe_recovery_rate <- recovery_rate
  
  #Vaccination
  no_vacc_changes <- length(tt_vaccination_coverage)
  
  #R0
  no_R0_changes <- length(tt_R0)
  
  #Seeding
  no_seeded_changes <- length(tt_seeded)
  seeded <- check_and_format_input(seeded, no_seeded_changes, n_age, n_vacc, n_risk)
  
  #Format initial population and infections
  N0 <- check_and_format_input(N0, n_age, n_vacc, n_risk)
  I0 <- check_and_format_input(I0, n_age, n_vacc, n_risk)
  
  #Prop severe
  prop_severe <- check_and_format_input(prop_severe, n_age, n_vacc, n_risk)
  
  #Vaccination
  vaccination_coverage <- check_and_format_input(vaccination_coverage, no_vacc_changes, n_age, n_vacc, n_risk)
  age_vaccination_beta_modifier <- check_and_format_input(age_vaccination_beta_modifier, n_age, n_vacc, n_risk)
  waning_rate <- check_and_format_input(waning_rate, n_age, n_vacc)
  
  #Births, deaths, aging
  if(is.null(repro_high)) repro_high <- n_age
  initial_background_death <- check_and_format_input(initial_background_death, n_age, n_risk)
  crude_birth <- check_and_format_input(crude_birth, length(tt_birth_changes), n_risk)
  crude_death <- check_and_format_input(crude_death, length(tt_birth_changes), n_age, n_risk)
  no_birth_changes = length(tt_birth_changes)
  no_death_changes = length(tt_death_changes)
  aging_rate <- check_and_format_input(aging_rate, n_age)
  #Final aging rate must be 0
  aging_rate[length(aging_rate)] <- 0
  
  
  # Export list -------------------------------------------------------------
  
  export_list <- list(
    
    #Dimensions
    n_age = n_age,
    n_vacc = n_vacc,
    n_risk = n_risk,
    
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
    #Natural protection waning
    delta = delta,
    
    #Vaccination
    vaccination_coverage = vaccination_coverage,
    tt_vaccination_coverage = tt_vaccination_coverage,
    no_vacc_changes = no_vacc_changes,
    age_vaccination_beta_modifier = age_vaccination_beta_modifier,
    waning_rate = waning_rate,
    
    #R0
    R0 = R0,
    tt_R0 = tt_R0,
    no_R0_changes = no_R0_changes,
    seeded = seeded,
    tt_seeded = tt_seeded,
    no_seeded_changes = no_seeded_changes, 
    
    #Births, deaths and aging
    # Aging rate by compartment
    aging_rate = aging_rate,
    # Set initial background death rate
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
    protection_weight_vacc = protection_weight_vacc,
    protection_weight_rec = protection_weight_rec,
    #Age group where maternal protection ends
    age_maternal_protection_ends = age_maternal_protection_ends,
    #Reproductive ages
    repro_low = repro_low,
    repro_high = repro_high
    
  )
  
  # Checks and balances -----------------------------------------------------
  
  #These must be above 0 and an integer
  above_zero_and_an_integer <- export_list[c("n_age", "n_vacc", "n_risk", "age_maternal_protection_ends", "repro_low", "repro_high")]
  
  #Check if the sum is above 0 and an integer
  sum_above_zero_and_an_integer <- export_list[c("N0")]
  
  #These must be non-negative and integers
  non_neg_int <- export_list[c("tt_vaccination_coverage", "no_vacc_changes", "tt_R0", "no_R0_changes", "tt_birth_changes", "tt_death_changes", "no_birth_changes", "no_death_changes", "repro_low", "repro_high", "I0", "seeded", "tt_seeded")]
  
  #These must be probabilities
  probability <- export_list[c("incubation_rate", "recovery_rate", "severe_recovery_rate", "prop_severe", "prop_complications", "vaccination_coverage", "age_vaccination_beta_modifier", "initial_background_death", "crude_birth", "crude_death", "protection_weight_vacc", "protection_weight_rec", "aging_rate", "contact_matrix", "waning_rate", "delta")]
  
  #Non-negative
  non_negative <- export_list[c("R0")]
  
  #Not above a reference value
  above_value_age <- export_list[c("repro_low", "repro_high")]

  #Run checks
  a <- check_parameter(above_zero_and_an_integer, check_above_zero = T, check_integer = T)
  b <- check_parameter(probability, check_probability = T)
  c <- check_parameter(non_negative, check_non_negative = T)
  d <- check_parameter(above_value_age, check_above_zero = T, check_not_above_reference_value = n_age)
  e <- check_parameter(non_neg_int, check_non_negative = T, check_integer = T)
  f <- check_parameter(sum_above_zero_and_an_integer, check_if_sum_above_zero = T, check_integer = T)
  
  #All tests
  all_failures <- rbind(a, b, c, d, e, f)
  
  if(nrow(all_failures) > 0) {
    warning("The following parameters failed checks:\n", paste(capture.output(print(all_failures, row.names = FALSE)), collapse = "\n"))
  } else export_list
    
}