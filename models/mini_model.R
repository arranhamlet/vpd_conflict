deriv(S[, ]) <- N[i, j] * birth_rate
  
initial(S[, ]) <- N[i, j]

N <- parameter()
birth_rate <- parameter(1/(80 * 365))
n_age <- parameter(1)
n_vacc <- parameter(1)

dim(S) <- c(n_age, n_vacc)
dim(N) <- c(n_age, n_vacc)