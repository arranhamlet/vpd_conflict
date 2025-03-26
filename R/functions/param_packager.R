#' Parameter Packager Function
#'
#' This function packages and formats all input parameters for an infectious disease transmission model
#' with age, vaccination, and risk stratification. It includes support for demographic processes such as
#' births, deaths, aging, and risk group movement, as well as interpolated time-varying parameters.
#'
#' @param n_age Number of age groups.
#' @param n_vacc Number of vaccination strata.
#' @param n_risk Number of risk groups.
#' @param N0 Initial population size (array or scalar).
#' @param I0 Initial number of infections (array or scalar).
#' @param contact_matrix Contact matrix defining age-specific contact patterns.
#' @param incubation_rate Incubation rate (E to I transition).
#' @param recovery_rate Recovery rate for non-severe infections.
#' @param severe_recovery_rate Recovery rate for severe infections (defaults to `recovery_rate` if `NULL`).
#' @param prop_severe Proportion of infections that become severe (can be scalar or array).
#' @param prop_complications Proportion of severe infections that result in complications.
#' @param delta Waning rate of natural protection.
#' @param vaccination_coverage Time-varying vaccination coverage array.
#' @param tt_vaccination_coverage Time points for changes in vaccination coverage.
#' @param age_vaccination_beta_modifier Modifies transmission based on age and vaccination status.
#' @param waning_rate Time- and group-specific waning rate for vaccination-induced immunity.
#' @param R0 Basic reproduction number.
#' @param tt_R0 Time points at which R0 changes.
#' @param seeded Initial number of seeded infections (can be time-varying).
#' @param tt_seeded Time points for seeding changes.
#' @param aging_rate Rate of aging between age compartments (last age group should be 0).
#' @param initial_background_death Background death rate at baseline.
#' @param simp_birth_death If set to 1, births and deaths are linked to background rates.
#' @param tt_birth_changes Time points when the birth rate changes.
#' @param tt_death_changes Time points when the death rate changes.
#' @param crude_birth Crude birth rate (used if `simp_birth_death != 1`).
#' @param crude_death Crude death rate (used if `simp_birth_death != 1`).
#' @param protection_weight_vacc Weight of vaccine-derived maternal protection by age.
#' @param protection_weight_rec Weight of natural infection-derived maternal protection by age.
#' @param age_maternal_protection_ends Age group at which maternal protection ends.
#' @param repro_low Lowest reproductive age group.
#' @param repro_high Highest reproductive age group (defaults to `n_age` if `NULL`).
#' @param tt_moving_risk Time points for risk group movement transitions.
#' @param moving_risk_values Total number of people moving between risk groups (by age/vacc/risk), accepts a data.frame of dimensions 1-4, and values.
#' @param moving_risk_distribution_values Proportion of movers going to each risk group (by age/vacc/risk), accepts a data.frame of dimensions 1-4, and values.
#'
#' @return A named list of formatted and validated parameters for input into a stochastic infectious disease model.
#'         The function also performs internal checks on validity and consistency of the inputs.
#'
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
  incubation_rate = 1/5,
  #Recovery rate
  recovery_rate = 1/14,
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
  R0 = 2,
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
  repro_high = NULL,
  
  #Movement between risk groups
  tt_moving_risk = 0,
  moving_risk_values = 0,
  moving_risk_distribution_values = 0
  
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
  
  #Moving between risk groups
  no_moving_risk_changes <- length(tt_moving_risk)
  
  moving_risk_values <- if(all(moving_risk_values == 0)) check_and_format_input(no_moving_risk_changes, no_vacc_changes, n_age, n_vacc, n_risk) else {
  generate_array_df(
    dim1 = no_moving_risk_changes, 
    dim2 = n_age, 
    dim3 = n_vacc, 
    dim4 = n_risk, 
    default_value = 0,
    updates = moving_risk_values
  ) %>%
    df_to_array
  }
  
  moving_risk_distribution_values <- if(all(moving_risk_distribution_values == 0)) check_and_format_input(no_moving_risk_changes, no_vacc_changes, n_age, n_vacc, n_risk) else {
    generate_array_df(
    dim1 = no_moving_risk_changes, 
    dim2 = n_age, 
    dim3 = n_vacc, 
    dim4 = n_risk, 
    default_value = 0,
    updates = moving_risk_distribution_values
  ) %>%
    df_to_array
  }
  
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
    repro_high = repro_high,
    #Movement between risk groups
    tt_moving_risk = tt_moving_risk ,
    no_moving_risk_changes = no_moving_risk_changes, 
    moving_risk_values = moving_risk_values,
    moving_risk_distribution_values = moving_risk_distribution_values
    
  )
  
  # Checks and balances -----------------------------------------------------
  
  #These must be above 0 and an integer
  above_zero_and_an_integer <- export_list[c("n_age", "n_vacc", "n_risk", "age_maternal_protection_ends", "repro_low", "repro_high")]
  
  #Check if the sum is above 0 and an integer
  sum_above_zero_and_an_integer <- export_list[c("N0")]
  
  #These must be non-negative and integers
  non_neg_int <- export_list[c("tt_vaccination_coverage", "no_vacc_changes", "tt_R0", "no_R0_changes", "tt_birth_changes", "tt_death_changes", "no_birth_changes", "no_death_changes", "repro_low", "repro_high", "I0", "seeded", "tt_seeded", "tt_moving_risk", "no_moving_risk_changes")]
  
  #These must be probabilities
  probability <- export_list[c("incubation_rate", "recovery_rate", "severe_recovery_rate", "prop_severe", "prop_complications", "vaccination_coverage", "age_vaccination_beta_modifier", "initial_background_death", "crude_birth", "crude_death", "protection_weight_vacc", "protection_weight_rec", "aging_rate", "contact_matrix", "waning_rate", "delta", "moving_risk_values", "moving_risk_distribution_values")]
  
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