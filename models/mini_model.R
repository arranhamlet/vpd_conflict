
# Compartments ------------------------------------------------------------

deriv(S) <- Births - b * S - beta * S * I / N + delta * R
deriv(E) <- beta * S * I / N - (b + gamma) * E
deriv(I) <- gamma * E - (b + sigma + alpha) * I
deriv(R) <- sigma * I - (b + delta) * R


# Initial compartment values ----------------------------------------------

initial(S) <- N0 - I0
initial(E) <- 0
initial(I) <- I0
initial(R) <- 0


# User parameter values --------------------------------------------------------

#Initial total population
N0 <- parameter(100)
#Initial infected population
I0 <- parameter(1)
#Incubation rate
gamma <- parameter(1)
#Recovery rate
sigma <- parameter(1) 
#Disease specific mortality
alpha <- parameter(0)
#Waning antibody rate
delta <- parameter(0)
#Background death rate
b <- parameter(2.6e-4)
#R0
R0 <- parameter(5)


# Calculated parameters ---------------------------------------------------

#Beta
beta <- R0 * ((b + gamma) / gamma) * (b + alpha + sigma)
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







