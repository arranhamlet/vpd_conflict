
# Compartments ------------------------------------------------------------

deriv(S) <- Births - b * S - beta * S * (I + Is) / N + delta * R + delta * Rc
deriv(E) <- beta * S * (I + Is) / N - (b + incubation_rate * (1 - prop_severe)) * E
deriv(I) <- incubation_rate * E - (b + recovery_rate + alpha) * I
deriv(R) <- recovery_rate * I - (b + delta) * R + Is * (1 - prop_complications)

deriv(Is) <- E * incubation_rate * prop_severe - Is * (severe_recovery_rate + b + severe_death_rate)
deriv(Rc) <- Is * severe_recovery_rate * prop_complications - Rc * (b + delta)

# Initial compartment values ----------------------------------------------

initial(S) <- N0 - I0
initial(E) <- 0
initial(I) <- I0
initial(R) <- 0
initial(Is) <- 0
initial(Rc) <- 0

# User parameter values --------------------------------------------------------

#Initial total population
N0 <- parameter(100)
#Initial infected population
I0 <- parameter(1)
#Incubation rate
incubation_rate <- parameter(1)
#Recovery rate
recovery_rate <- parameter(1) 
#Disease specific mortality
alpha <- parameter(0)
#Waning antibody rate
delta <- parameter(0)
#Background death rate
b <- parameter(2.6e-4)
#R0
R0 <- parameter(5)
#Proportion of cases that are severe
prop_severe <- parameter(0)
#Severe case recovery rate
severe_recovery_rate <- parameter(1)
#Severe death rate
severe_death_rate <- parameter(0)
#Proportion of cases that have complications
prop_complications <- parameter(0)

# Calculated parameters ---------------------------------------------------

#Beta
beta <- R0 * ((b + incubation_rate) / incubation_rate) * (b + alpha + recovery_rate)
#Number of births
Births <- b * N0
#R-effective (Re)
R_effective <- R0 * S/N
#Total population
N <- S + E + I + R


# Output ------------------------------------------------------------------
#Output R-effective
output(reff) <- R_effective
#Output total population
output(pop) <- N







