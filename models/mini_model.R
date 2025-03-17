
# Compartments ------------------------------------------------------------

deriv(S[, , ]) <- - background_death * S[i, j, k] - lambda[i, j, k] * S[i, j, k] + delta * R[i, j, k] + delta * Rc[i, j, k] + aging_into_S[i, j, k] - aging_out_of_S[i, j, k]

deriv(E[, , ]) <- lambda[i, j, k] * S[i, j, k] - (background_death + incubation_rate) * E[i, j, k] + aging_into_E[i, j, k] - aging_out_of_E[i, j, k]

deriv(I[, , ]) <- E[i, j, k] * incubation_rate * (1 - prop_severe[i, j, k]) - (background_death + recovery_rate + alpha) * I[i, j, k] + aging_into_I[i, j, k] - aging_out_of_I[i, j, k]

deriv(R[, , ]) <- recovery_rate * I[i, j, k] - (background_death + delta) * R[i, j, k] + Is[i, j, k] * severe_recovery_rate * (1 - prop_complications) + aging_into_R[i, j, k] - aging_out_of_R[i, j, k]

deriv(Is[, , ]) <- E[i, j, k] * incubation_rate * prop_severe[i, j, k] - Is[i, j, k] * (severe_recovery_rate + background_death + severe_death_rate) + aging_into_Is[i, j, k] - aging_out_of_Is[i, j, k]

deriv(Rc[, , ]) <- Is[i, j, k] * severe_recovery_rate * prop_complications - Rc[i, j, k] * (background_death + delta) + aging_into_Rc[i, j, k] - aging_out_of_Rc[i, j, k]

# Add in births and aging
aging_into_S[1, 1, ] <- Births[k]

aging_into_S[2:n_age, , ] <- S[i - 1, j, k] * aging_rate[i-1]
aging_out_of_S[1:(n_age - 1), , ] <- S[i, j, k] * aging_rate[i]

aging_into_E[2:n_age, , ] <- E[i - 1, j, k] * aging_rate[i-1]
aging_out_of_E[1:(n_age - 1), , ] <- E[i, j, k] * aging_rate[i]

aging_into_I[2:n_age, , ] <- I[i - 1, j, k] * aging_rate[i-1]
aging_out_of_I[1:(n_age - 1), , ] <- I[i, j, k] * aging_rate[i]

aging_into_R[2:n_age, , ] <- R[i - 1, j, k] * aging_rate[i-1]
aging_out_of_R[1:(n_age - 1), , ] <- R[i, j, k] * aging_rate[i]

aging_into_Is[2:n_age, , ] <- Is[i - 1, j, k] * aging_rate[i-1]
aging_out_of_Is[1:(n_age - 1), , ] <- Is[i, j, k] * aging_rate[i]

aging_into_Rc[2:n_age, , ] <- Rc[i - 1, j, k] * aging_rate[i-1]
aging_out_of_Rc[1:(n_age - 1), , ] <- Rc[i, j, k] * aging_rate[i]

# Initial compartment values ----------------------------------------------

initial(S[, , ]) <- N0[i, j, k] - I0[i, j, k]
initial(E[, , ]) <- 0
initial(I[, , ]) <- I0[i, j, k]
initial(R[, , ]) <- 0
initial(Is[, , ]) <- 0
initial(Rc[, , ]) <- 0

# User parameter values --------------------------------------------------------

#Initial total population
N0 <- parameter()
#Initial infected population
I0 <- parameter()
#Incubation rate
incubation_rate <- parameter()
#Recovery rate
recovery_rate <- parameter() 
#Disease specific mortality
alpha <- parameter(0)
#Waning antibody rate
delta <- parameter(0)
#Background death rate
background_death <- parameter()
#Birth rate
# birth_rate <- parameter()
#R0
R0 <- parameter()
#Set the number of times R0 changes
no_R0_changes <- parameter()
#Define the times when R0 changes
tt_R0 <- parameter()
#Interpolate R0
t_R0 <- interpolate(tt_R0, R0, "constant")
#Proportion of cases that are severe
prop_severe <- parameter()
#Severe case recovery rate
severe_recovery_rate <- parameter()
#Severe death rate
severe_death_rate <- parameter(0)
#Proportion of cases that have complications
prop_complications <- parameter(0)
#Beta modifier for age and vaccination
age_vaccination_beta_modifier <- parameter()
#Number of age compartments
n_age <- parameter(1)
#Number of vaccination compartments
n_vacc <- parameter(1)
#Number of vulnerable population compartments
n_vulnerable <- parameter(1)
#Contact matrix
contact_matrix <- parameter()
#Aging rate
aging_rate <- parameter()
#Reproductive lower limit
repro_low <- parameter()
#Reproductive upper limit
repro_high <- parameter()
#Maternal antibody protection
age_maternal_protection_ends <- parameter()
#Maternal antibody protection
protection_weight <- parameter()

# Calculated parameters ---------------------------------------------------

#Calculate infectious period
infectious_period[, , ] <- if((severe_recovery_rate + severe_death_rate + background_death) == 0 || (recovery_rate + alpha + background_death) == 0) 0 else (1 - prop_severe[i, j, k]) / (recovery_rate + alpha + background_death) + prop_severe[i, j, k] / (severe_recovery_rate + severe_death_rate + background_death)
#Calculate beta from the R0 and infectious period
beta[, , ] <- if(infectious_period[i, j, k] == 0) 0 else t_R0 / infectious_period[i, j, k]

#Update with vaccination and age mediation
beta_updated[, , ] <- age_vaccination_beta_modifier[i, j, k] * beta[i, j, k]
#Update with maternal protection to first groups
beta_updated[1:age_maternal_protection_ends, , ] <- beta_updated[i, j, k] * (1 - protection_weight[i] * prop_vaccinated[k])

# output(protection_weight2) <- sum(protection_weight)
# output(prop_vaccinated2) <- sum(prop_vaccinated)

#Calculate the force of infection - using a contact matrix
lambda[, , ] <- sum(contact_matrix[i, ]) * sum(beta_updated[, j, k]) * (sum(I[, j, k]) + sum(Is[, j, k])) / N

#Calculate Reff in two parts due to Odin
S_eff[, , ] <- S[i, j, k] * age_vaccination_beta_modifier[i, j, k]
R_effective <- t_R0 * sum(S_eff) / N

#Total population
N <- sum(S) + sum(E) + sum(I) + sum(R) + sum(Is) + sum(Rc)
Npop_vulnerable[] <- sum(S[, , i]) + sum(E[, , i]) + sum(I[, , i]) + sum(R[, , i]) + sum(Is[, , i]) + sum(Rc[, , i])

#Number of births
reproductive_population[] <- sum(S[repro_low:repro_high, , i]) + 
  sum(E[repro_low:repro_high, , i]) + 
  sum(I[repro_low:repro_high, , i]) + 
  sum(R[repro_low:repro_high, , i])


birth_rate[] <- if(reproductive_population[i] == 0) 0 else (Npop_vulnerable[i] * background_death)/reproductive_population[i]

Births[] <-  birth_rate[i] * reproductive_population[i]

# Proportion of mothers who confer maternal antibodies
vaccinated_mums[] <- sum(S[repro_low:repro_high, 2:n_vacc, i]) + sum(E[repro_low:repro_high, 2:n_vacc, i]) + sum(I[repro_low:repro_high, 2:n_vacc, i]) + sum(R[repro_low:repro_high, 2:n_vacc, i]) + sum(Is[repro_low:repro_high, 2:n_vacc, i]) + sum(Rc[repro_low:repro_high, 2:n_vacc, i])

prop_vaccinated[] <- if(reproductive_population[i] == 0) 0 else vaccinated_mums[i]/reproductive_population[i]

# Dimensions --------------------------------------------------------------

dim(S) <- c(n_age, n_vacc, n_vulnerable)
dim(E) <- c(n_age, n_vacc, n_vulnerable)
dim(I) <- c(n_age, n_vacc, n_vulnerable)
dim(R) <- c(n_age, n_vacc, n_vulnerable)
dim(Is) <- c(n_age, n_vacc, n_vulnerable)
dim(Rc) <- c(n_age, n_vacc, n_vulnerable)
dim(N0) <- c(n_age, n_vacc, n_vulnerable)
dim(I0) <- c(n_age, n_vacc, n_vulnerable)
dim(beta_updated) <- c(n_age, n_vacc, n_vulnerable)
dim(age_vaccination_beta_modifier) <- c(n_age, n_vacc, n_vulnerable)
dim(prop_severe) <- c(n_age, n_vacc, n_vulnerable)
dim(beta) <- c(n_age, n_vacc, n_vulnerable)
dim(infectious_period) <- c(n_age, n_vacc, n_vulnerable)
dim(lambda) <- c(n_age, n_vacc, n_vulnerable)
dim(tt_R0) <- no_R0_changes
dim(R0) <- no_R0_changes
dim(S_eff) <- c(n_age, n_vacc, n_vulnerable)
dim(contact_matrix) <- c(n_age, n_age)
dim(aging_rate) <- n_age
dim(aging_into_S) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_out_of_S) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_into_E) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_out_of_E) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_into_I) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_out_of_I) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_into_R) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_out_of_R) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_into_Is) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_out_of_Is) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_into_Rc) <- c(n_age, n_vacc, n_vulnerable)
dim(aging_out_of_Rc) <- c(n_age, n_vacc, n_vulnerable)
dim(Births) <- n_vulnerable
dim(reproductive_population) <- n_vulnerable
dim(birth_rate) <- n_vulnerable
dim(Npop_vulnerable) <- n_vulnerable
dim(prop_vaccinated) <- n_vulnerable
dim(vaccinated_mums) <- n_vulnerable
dim(protection_weight) <- age_maternal_protection_ends


# Output ------------------------------------------------------------------
#Output R-effective
output(reff) <- R_effective
#Output total population
output(pop) <- N
output(born) <- Births[1]
output(died) <- sum(S) * background_death + sum(E) * background_death + sum(I) * background_death + sum(R) * background_death + sum(Is) * background_death + sum(Rc) * background_death 

output(lamb) <- sum(lambda)
output(infy) <- sum(infectious_period)
output(beta1) <- sum(beta)
output(beta2) <- sum(beta_updated)

  