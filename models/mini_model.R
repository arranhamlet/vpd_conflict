
# Compartments ------------------------------------------------------------

deriv(S[, ]) <- Births - b * S[i, j] - beta_updated[i, j] * S[i, j] * (sum(I) + sum(Is)) / N + delta * R[i, j] + delta * Rc[i, j]
deriv(E[, ]) <- beta_updated[i, j] * S[i, j] * (sum(I) + sum(Is)) / N - (b + incubation_rate) * E[i, j]
deriv(I[, ]) <- E[i, j] * incubation_rate * (1 - prop_severe[i, j]) - (b + recovery_rate + alpha) * I[i, j]
deriv(R[, ]) <- recovery_rate * I[i, j] - (b + delta) * R[i, j] + Is[i, j] * severe_recovery_rate * (1 - prop_complications)

deriv(Is[, ]) <- E[i, j] * incubation_rate * prop_severe[i, j] - Is[i, j] * (severe_recovery_rate + b + severe_death_rate)
deriv(Rc[, ]) <- Is[i, j] * severe_recovery_rate * prop_complications - Rc[i, j] * (b + delta)


# Initial compartment values ----------------------------------------------

initial(S[, ]) <- N0[i, j] - I0[i, j]
initial(E[, ]) <- 0
initial(I[, ]) <- I0[i, j]
initial(R[, ]) <- 0
initial(Is[, ]) <- 0
initial(Rc[, ]) <- 0


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
b <- parameter()
#R0
R0 <- parameter()
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

# Calculated parameters ---------------------------------------------------

#Beta
infectious_period[, ] <- (1 - prop_severe[i, j]) / (recovery_rate + alpha + b) + prop_severe[i, j] / (severe_recovery_rate + severe_death_rate + b)

beta[, ] <- R0 / infectious_period[i, j]



beta <- R0 / sum(infectious_period)




#Beta but with vaccination and age mediation
beta_updated[, ] <- age_vaccination_beta_modifier[i, j] * beta

#Number of births
Births <- b * N
#R-effective (Re) - Need to do in two parts because of the age_vaccination modifier
S_age_vacc_modified[,] <- S[i, j] * age_vaccination_beta_modifier[i, j]
R_effective <- R0 * sum(S_age_vacc_modified)/N

#Total population
N <- sum(S) + sum(E) + sum(I) + sum(R) + sum(Is) + sum(Rc)
#Number of age compartments
n_age <- parameter(1)
#Number of vaccination compartments
n_vacc <- parameter(1)


# Dimensions --------------------------------------------------------------

dim(S) <- c(n_age, n_vacc)
dim(E) <- c(n_age, n_vacc)
dim(I) <- c(n_age, n_vacc)
dim(R) <- c(n_age, n_vacc)
dim(Is) <- c(n_age, n_vacc)
dim(Rc) <- c(n_age, n_vacc)
dim(N0) <- c(n_age, n_vacc)
dim(I0) <- c(n_age, n_vacc)
dim(beta_updated) <- c(n_age, n_vacc)
dim(S_age_vacc_modified) <- c(n_age, n_vacc)
dim(age_vaccination_beta_modifier) <- c(n_age, n_vacc)
dim(prop_severe) <- c(n_age, n_vacc)
dim(beta) <- c(n_age, n_vacc)
dim(infectious_period) <- c(n_age, n_vacc)

# Output ------------------------------------------------------------------
#Output R-effective
output(reff) <- R_effective
#Output total population
output(pop) <- N

