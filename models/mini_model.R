

deriv(S[, ]) <- N * birth_rate - beta * (sum(I[i, j]) * sum(S[i, j])/N)
deriv(E[, ]) <- beta * (sum(I[i, j]) * sum(S[i, j])/N) - E[i, j] * 1/incubation_time
deriv(I[, ]) <- E[i, j] * 1/incubation_time - I[i, j] * 1/recovery_time
deriv(R[, ]) <- I[i, j] * 1/recovery_time

initial(S[, ]) <- initial_population[i, j]
initial(E[, ]) <- 0
initial(I[, ]) <- init_inf
initial(R[, ]) <- 0

initial_population <- parameter()
N <- sum(S) + sum(E) + sum(I) + sum(R)
output(total_pop) <- N

birth_rate <- parameter(1/(80 * 365))
n_age <- parameter(1)
n_vacc <- parameter(1)
beta <- R0 * 1/recovery_time * (1/incubation_time + 1/recovery_time)/(1/incubation_time)
init_inf <- parameter(1)

R0 <- parameter(2)
recovery_time <- parameter(14)
incubation_time <- parameter(5)

dim(S) <- c(n_age, n_vacc)
dim(E) <- c(n_age, n_vacc)
dim(I) <- c(n_age, n_vacc)
dim(R) <- c(n_age, n_vacc)
dim(initial_population) <- c(n_age, n_vacc)
