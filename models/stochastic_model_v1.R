
# Compartments ------------------------------------------------------------

update(S[, , ]) <- S[i, j, k] + waning_R[i, j, k] + waning_Rc[i, j, k] + aging_into_S[i, j, k] - aging_out_of_S[i, j, k] + vaccinating_into_S[i, j, k] - vaccinating_out_of_S[i, j, k] - lambda_S[i, j,k ] - S_death[i, j, k]

update(E[, , ]) <- E[i, j, k] + lambda_S[i, j, k] - incubated[i, j, k] + aging_into_E[i, j, k] - aging_out_of_E[i, j, k] + vaccinating_into_E[i, j, k] - vaccinating_out_of_E[i, j, k] - E_death[i, j, k]

update(I[, , ]) <- I[i, j, k] + into_I[i, j, k] + aging_into_I[i, j, k] - aging_out_of_I[i, j, k] + vaccinating_into_I[i, j, k] - vaccinating_out_of_I[i, j, k] - recovered_I_to_R[i, j, k] - I_death[i, j, k]

update(Is[, , ]) <- Is[i, j, k] + into_Is[i, j, k] - recovered_from_Is[i, j, k] + aging_into_Is[i, j, k] - aging_out_of_Is[i, j, k] + vaccinating_into_Is[i, j, k] - vaccinating_out_of_Is[i, j, k] - Is_death[i, j, k]

update(R[, , ]) <- R[i, j, k] + recovered_I_to_R[i, j, k] + recovered_Is_to_R[i, j, k] - waning_R[i, j, k] + aging_into_R[i, j, k] - aging_out_of_R[i, j, k] + vaccinating_into_R[i, j, k] - vaccinating_out_of_R[i, j, k] - R_death[i, j, k]

update(Rc[, , ]) <- Rc[i, j, k] + recovered_Is_to_Rc[i, j, k] - waning_Rc[i, j, k] + aging_into_Rc[i, j, k] - aging_out_of_Rc[i, j, k] + vaccinating_into_Rc[i, j, k] - vaccinating_out_of_Rc[i, j, k] - Rc_death[i, j, k]

#Death
S_death[, , ] <- if(S[i, j, k] <= 0) 0 else Binomial(S[i, j, k], max(min(background_death[i, k], 1), 0))
E_death[, , ] <- if(E[i, j, k] <= 0) 0 else Binomial(E[i, j, k], max(min(background_death[i, k], 1), 0))
I_death[, , ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k], max(min(background_death[i, k], 1), 0) + max(alpha, 0))
R_death[, , ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k], max(min(background_death[i, k], 1), 0))
Is_death[, , ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k], max(min(background_death[i, k], 1), 0) + max(severe_death_rate, 0))
Rc_death[, , ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k], max(min(background_death[i, k], 1), 0))

#S sampling
lambda_S[, , ] <- if(S[i, j, k] <= 0) 0 else Binomial(S[i, j, k], max(min(lambda[i, j, k], 1), 0))
waning_R[, , ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k], max(min(delta, 1), 0))
waning_Rc[, , ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k], max(min(delta, 1), 0))

#E sampling
incubated[, , ] <- if(E[i, j, k] <= 0) 0 else Binomial(E[i, j, k], max(min(incubation_rate, 1), 0))

#I sampling
into_I[, , ] <- if(incubated[i, j, k] <= 0) 0 else Binomial(incubated[i, j, k], max(min(1 - prop_severe[i, j, k], 1), 0))
into_Is[, , ] <- incubated[i, j, k] - into_I[i, j, k]
recovered_I_to_R[, , ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k], max(min(recovery_rate, 1), 0))

#Is sampling
recovered_from_Is[, , ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k], max(min(severe_recovery_rate, 1), 0))
recovered_Is_to_R[, , ] <- if(recovered_from_Is[i, j, k] <= 0) 0 else Binomial(recovered_from_Is[i, j, k], max(min(prop_complications, 1), 0))
recovered_Is_to_Rc[, , ] <- recovered_from_Is[i, j, k] - recovered_Is_to_R[i, j, k]

# Add in births and aging
aging_into_S[1, 1, ] <- Births[k]

aging_into_S[2:n_age, , ] <- if(S[i - 1, j, k] <= 0) 0 else Binomial(S[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_S[1:(n_age - 1), , ] <- if(S[i, j, k] <= 0) 0 else Binomial(S[i, j, k], max(min(aging_rate[i], 1), 0))

aging_into_E[2:n_age, , ] <- if(E[i - 1, j, k] <= 0) 0 else Binomial(E[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_E[1:(n_age - 1), , ] <- if(E[i, j, k] <= 0) 0 else Binomial(E[i, j, k], max(min(aging_rate[i], 1), 0))

aging_into_I[2:n_age, , ] <- if(I[i - 1, j, k] <= 0) 0 else Binomial(I[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_I[1:(n_age - 1), , ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k], max(min(aging_rate[i], 1), 0))

aging_into_R[2:n_age, , ] <- if(R[i - 1, j, k] <= 0) 0 else Binomial(R[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_R[1:(n_age - 1), , ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k], max(min(aging_rate[i], 1), 0))

aging_into_Is[2:n_age, , ] <- if(Is[i - 1, j, k] <= 0) 0 else Binomial(Is[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_Is[1:(n_age - 1), , ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k], max(min(aging_rate[i], 1), 0))

aging_into_Rc[2:n_age, , ] <- if(Rc[i - 1, j, k] <= 0) 0 else Binomial(Rc[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_Rc[1:(n_age - 1), , ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k], max(min(aging_rate[i], 1), 0))

#Set up times for changing vaccination coverage
tt_vaccination_coverage <- parameter()
no_vacc_changes <- parameter()
dim(tt_vaccination_coverage) <- no_vacc_changes

#Set up vaccination coverage changing over time
vaccination_coverage <- parameter()
dim(vaccination_coverage) <- c(no_vacc_changes, n_age, n_vacc, n_vulnerable)

#Interpolate
vaccination_prop <- interpolate(tt_vaccination_coverage, vaccination_coverage, "constant")

# Susceptible vaccination
vaccinating_into_S[, 1, ] <- if(S[i, j, k] <= 0) 0 else Binomial(S[i, j, k] , 0)
vaccinating_into_S[, 2:n_vacc, ] <- if(S[i, j - 1, k] <= 0) 0 else Binomial(S[i, j - 1, k] , max(min(vaccination_prop[i, j - 1, k], 1), 0))
vaccinating_out_of_S[, 1:(n_vacc - 1), ] <- if(S[i, j, k] <= 0) 0 else Binomial(S[i, j, k] , max(min(vaccination_prop[i, j, k], 1), 0))

# Exposed vaccination
vaccinating_into_E[, 1, ] <- if(E[i, j, k] <= 0) 0 else Binomial(E[i, j, k] , 0)
vaccinating_into_E[, 2:n_vacc , ] <- if(E[i, j - 1, k] <= 0) 0 else Binomial(E[i, j - 1, k] , max(min(vaccination_prop[i, j - 1, k], 1), 0))
vaccinating_out_of_E[, 1:(n_vacc - 1), ] <- if(E[i, j , k] <= 0) 0 else Binomial(E[i, j , k] , max(min(vaccination_prop[i, j, k], 1), 0))

# Infectious vaccination
vaccinating_into_I[, 1, ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k] , 0)
vaccinating_into_I[, 2:n_vacc , ] <- if(I[i, j - 1, k] <= 0) 0 else Binomial(I[i, j - 1, k] , max(min(vaccination_prop[i, j - 1, k], 1), 0))
vaccinating_out_of_I[,1:(n_vacc - 1) , ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k] , max(min(vaccination_prop[i, j, k], 1), 0))

# Recovered vaccination
vaccinating_into_R[, 1, ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k] , 0)
vaccinating_into_R[, 2:n_vacc , ] <- if(R[i, j - 1, k] <= 0) 0 else Binomial(R[i, j - 1, k] , max(min(vaccination_prop[i, j - 1, k], 1), 0))
vaccinating_out_of_R[, 1:(n_vacc - 1) , ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k] , max(min(vaccination_prop[i, j, k], 1), 0))

# Infectious severe vaccination
vaccinating_into_Is[, 1, ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k] , 0)
vaccinating_into_Is[, 2:n_vacc , ] <- if(Is[i, j - 1, k] <= 0) 0 else Binomial(Is[i, j - 1, k] , max(min(vaccination_prop[i, j - 1, k], 1), 0))
vaccinating_out_of_Is[, 1:(n_vacc - 1) , ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k] , max(min(vaccination_prop[i, j, k], 1), 0))

# Recovered complications vaccination
vaccinating_into_Rc[, 1, ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k] , 0)
vaccinating_into_Rc[, 2:n_vacc , ] <- if(Rc[i, j - 1, k] <= 0) 0 else Binomial(Rc[i, j - 1, k] , max(min(vaccination_prop[i, j - 1, k], 1), 0))
vaccinating_out_of_Rc[, 1:(n_vacc - 1) , ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k] , max(min(vaccination_prop[i, j, k], 1), 0))



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
initial_background_death <- parameter()
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
infectious_period[, , ] <- if((severe_recovery_rate + severe_death_rate + background_death[i, k]) <= 0 || (recovery_rate + alpha + background_death[i, k]) <= 0) 0 else (1 - prop_severe[i, j, k]) / (recovery_rate + alpha + background_death[i, k]) + prop_severe[i, j, k] / (severe_recovery_rate + severe_death_rate + background_death[i, k])
#Calculate beta from the R0 and infectious period
beta[, , ] <- if(infectious_period[i, j, k] <= 0) 0 else t_R0 / infectious_period[i, j, k]

#Update with vaccination and age mediation
beta_updated[, , ] <- age_vaccination_beta_modifier[i, j, k] * beta[i, j, k]
#Update with maternal protection to first groups
beta_updated[1:age_maternal_protection_ends, , ] <- beta_updated[i, j, k] * (1 - protection_weight[i] * prop_vaccinated[k])

#Calculate the force of infection - using a contact matrix
lambda[, , ] <- max(0, sum(contact_matrix[i, ]) * sum(beta_updated[, j, k]) * (sum(I[, j, k]) + sum(Is[, j, k])) / N)

#Calculate Reff in two parts due to Odin
S_eff[, , ] <- S[i, j, k] * age_vaccination_beta_modifier[i, j, k]
update(R_effective) <- t_R0 * sum(S_eff) / N
initial(R_effective) <- 0

#Total population
N <- sum(S) + sum(E) + sum(I) + sum(R) + sum(Is) + sum(Rc)

update(total_pop) <- N
initial(total_pop) <- sum(N0)

Npop_age_vulnerable[, ] <- sum(S[i, , j]) + sum(E[i, , j]) + sum(I[i, , j]) + sum(R[i, , j]) + sum(Is[i, , j]) + sum(Rc[i, , j])

dim(Npop_age_vulnerable) <- c(n_age, n_vulnerable)

#Number of births
reproductive_population[] <- sum(S[repro_low:repro_high, , i]) + 
  sum(E[repro_low:repro_high, , i]) + 
  sum(I[repro_low:repro_high, , i]) + 
  sum(R[repro_low:repro_high, , i])

Npop_background_death[, ] <- Binomial(Npop_age_vulnerable[i, j], max(min(background_death[i, j], 1), 0))
dim(Npop_background_death) <- c(n_age, n_vulnerable)

birth_rate[] <- if(reproductive_population[i] <= 0) 0 else sum(Npop_background_death[, i])/reproductive_population[i]

#Add in switch to decouple births and deaths when wanted
crude_birth <- parameter()
crude_death <- parameter()

no_birth_changes <- parameter()
no_death_changes <- parameter()

tt_birth_changes <- parameter()
tt_death_changes <- parameter()

dim(crude_birth) <- c(no_birth_changes, n_vulnerable)
dim(crude_death) <- c(no_birth_changes, n_age, n_vulnerable)

dim(tt_birth_changes) <- no_birth_changes
dim(tt_death_changes) <- no_death_changes

birth_int <- interpolate(tt_birth_changes, crude_birth, "constant")
death_int <- interpolate(tt_death_changes, crude_death, "constant")

dim(birth_int) <- n_vulnerable
dim(death_int) <- c(n_age, n_vulnerable)

simp_birth_death <- parameter(1)

Births[] <-  if(reproductive_population[i] <= 0) 0 else if(simp_birth_death == 1) Binomial(reproductive_population[i], max(min(birth_rate[i], 1), 0)) else Binomial(reproductive_population[i], max(min(birth_int[i], 1), 0))

background_death[, ]<- if(simp_birth_death == 1) max(min(initial_background_death[i, j], 1), 0) else max(min(death_int[i, j], 1), 0)

# Proportion of mothers who confer maternal antibodies
vaccinated_mums[] <- sum(S[repro_low:repro_high, 2:n_vacc, i]) + sum(E[repro_low:repro_high, 2:n_vacc, i]) + sum(I[repro_low:repro_high, 2:n_vacc, i]) + sum(R[repro_low:repro_high, 2:n_vacc, i]) + sum(Is[repro_low:repro_high, 2:n_vacc, i]) + sum(Rc[repro_low:repro_high, 2:n_vacc, i])

#Adding in R and Rc mums too
prop_vaccinated[] <- if(reproductive_population[i] <= 0) 0 else (vaccinated_mums[i] + sum(R[repro_low:repro_high, 1, i]) + sum(Rc[repro_low:repro_high, 1, i]))/reproductive_population[i]

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
# dim(Npop_vulnerable) <- n_vulnerable
dim(prop_vaccinated) <- n_vulnerable
dim(vaccinated_mums) <- n_vulnerable
dim(protection_weight) <- age_maternal_protection_ends

dim(vaccinating_into_S) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_out_of_S) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_into_E) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_out_of_E) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_into_I) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_out_of_I) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_into_R) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_out_of_R) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_into_Is) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_out_of_Is) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_into_Rc) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccinating_out_of_Rc) <- c(n_age, n_vacc, n_vulnerable)
dim(vaccination_prop) <- c(n_age, n_vacc, n_vulnerable)

dim(background_death) <- c(n_age, n_vulnerable)
dim(initial_background_death) <- c(n_age, n_vulnerable)

dim(lambda_S) <- c(n_age, n_vacc, n_vulnerable)
dim(waning_R) <- c(n_age, n_vacc, n_vulnerable)
dim(waning_Rc) <- c(n_age, n_vacc, n_vulnerable)
dim(incubated) <- c(n_age, n_vacc, n_vulnerable)
dim(into_I) <- c(n_age, n_vacc, n_vulnerable)
dim(into_Is) <- c(n_age, n_vacc, n_vulnerable)
dim(recovered_I_to_R) <- c(n_age, n_vacc, n_vulnerable)
dim(recovered_from_Is) <- c(n_age, n_vacc, n_vulnerable)
dim(recovered_Is_to_R) <- c(n_age, n_vacc, n_vulnerable)
dim(recovered_Is_to_Rc) <- c(n_age, n_vacc, n_vulnerable)

dim(S_death) <- c(n_age, n_vacc, n_vulnerable)
dim(E_death) <- c(n_age, n_vacc, n_vulnerable)
dim(I_death) <- c(n_age, n_vacc, n_vulnerable)
dim(R_death) <- c(n_age, n_vacc, n_vulnerable)
dim(Is_death) <- c(n_age, n_vacc, n_vulnerable)
dim(Rc_death) <- c(n_age, n_vacc, n_vulnerable)

