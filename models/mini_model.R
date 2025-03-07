# variables
deriv(S) <- Births - b * S - beta * S * I / N + delta * R
deriv(E) <- beta * S * I / N - (b + gamma) * E
deriv(I) <- gamma * E - (b + sigma + alpha) * I
deriv(R) <- sigma * I - (b + delta) * R

# initial conditions of the variables
initial(S) <- N0 - I0
initial(E) <- 0
initial(I) <- I0
initial(R) <- 0

N <- S + E + I + R
output(pop) <- N

# parameter values
N0 <- parameter(1e7)           # total population size
I0 <- parameter(1)             # num infectious cases at start of epidemic
sigma <- parameter(1)         # recovery rate (1/mean duration infectiousness)
gamma <- parameter(1)
alpha <- parameter(0)
delta <- parameter(0)          # waning antibody rate
b <- parameter(2.6e-4)         # death rate (average life expectancy of 1 year or 52 weeks)
R0 <- parameter(5)

Births <- b * N0          # number of births (for a constant population size)
beta <- R0 * ((b + gamma) / gamma) * (b + alpha + sigma)









