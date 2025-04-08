# Initial compartment values ----------------------------------------------

#Compartments
initial(S[, , ]) <- N0[i, j, k]
initial(E[, , ]) <- 0
initial(I[, , ]) <- 0
initial(R[, , ]) <- 0
initial(Is[, , ]) <- 0
initial(Rc[, , ]) <- 0

# Compartments ------------------------------------------------------------

# Update compartments
update(S[, , ]) <- max(S[i, j, k] + migration_S[i, j, k] * pos_neg_migration, 0)

update(E[, , ]) <- max(E[i, j, k] + migration_E[i, j, k] * pos_neg_migration, 0)

update(I[, , ]) <- max(I[i, j, k] + migration_I[i, j, k] * pos_neg_migration, 0)

update(R[, , ]) <- max(R[i, j, k] + migration_R[i, j, k] * pos_neg_migration, 0)

update(Is[, , ]) <- max(Is[i, j, k] + migration_Is[i, j, k] * pos_neg_migration, 0)

update(Rc[, , ]) <- max(Rc[i, j, k] + migration_Rc[i, j, k] * pos_neg_migration, 0)

update(total_pop) <- sum(S) + sum(E) + sum(I) + sum(R) + sum(Is) + sum(Rc)
initial(total_pop) <- sum(N0)

# Parameters to control movement between risk groups
migration <- interpolate(tt_migration, migration_in_number, "constant")
migration_distribution <- interpolate(tt_migration, migration_distribution_values, "constant")

update(input_migration_interpolate[, , , ]) <- migration_in_number[i, j, k, l]
dim(input_migration_interpolate) <- c(no_migration_changes, n_age, n_vacc, n_risk)
initial(input_migration_interpolate[, , , ]) <- 0

update(output_migration_interpolate[, , ]) <- migration[i, j, k]
dim(output_migration_interpolate) <- c(n_age, n_vacc, n_risk)
initial(output_migration_interpolate[, , ]) <- 0

#Positive or negative flow
pos_neg_migration <- if(sum(migration) < 0) -1 else 1
migration_adjusted[, , ] <- migration[i, j, k] * pos_neg_migration

# Moving INTO each compartment with specified distribution
migration_S[, , ] <- if(migration_adjusted[i, j, k] <= 0 || migration_distribution[1, i, j, k] == 0) 0 else Binomial(migration_adjusted[i, j, k], migration_distribution[1, i, j, k])

migration_E[, , ] <- if(migration_adjusted[i, j, k] - migration_S[i, j, k] <= 0 || migration_distribution[2, i, j, k] == 0) 0 else Binomial(migration_adjusted[i, j, k] - migration_S[i, j, k], migration_distribution[2, i, j, k])

migration_I[, , ] <- min(if(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] <= 0 || migration_distribution[3, i, j, k] == 0) 0 else Binomial(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k], migration_distribution[3, i, j, k]), migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k])

migration_R[, , ] <- min(if(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k] <= 0 || migration_distribution[4, i, j, k] == 0) 0 else Binomial(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k], migration_distribution[4, i, j, k]), migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k])

migration_Is[, , ] <- min(if(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k] - migration_R[i, j, k] <= 0 || migration_distribution[5, i, j, k] == 0) 0 else Binomial(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k] - migration_R[i, j, k], migration_distribution[5, i, j, k]), migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k] - migration_R[i, j, k])

migration_Rc[, , ] <- min(if(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k] - migration_R[i, j, k] - migration_Is[i, j, k] <= 0 || migration_distribution[6, i, j, k] == 0) 0 else Binomial(migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k] - migration_R[i, j, k] - migration_Is[i, j, k], migration_distribution[6, i, j, k]), migration_adjusted[i, j, k] - migration_S[i, j, k] - migration_E[i, j, k] - migration_I[i, j, k] - migration_R[i, j, k] - migration_Is[i, j, k])

migration_left <- sum(migration_adjusted) - sum(migration_S) - sum(migration_E) - sum(migration_I) - sum(migration_R) - sum(migration_Is) - sum(Rc)

update(migration_leftover) <- migration_left
initial(migration_leftover) <- 0

# User parameter values --------------------------------------------------------

#Compartment dimensions
#Number of age compartments
n_age <- parameter(1)
#Number of vaccination compartments
n_vacc <- parameter(1)
#Number of risk population compartments
n_risk <- parameter(1)

#Initial total population
N0 <- parameter()

#Migration parameters
no_migration_changes <- parameter()
tt_migration <- parameter()
migration_in_number <- parameter()
migration_distribution_values <- parameter()


# Dimensions --------------------------------------------------------------

dim(S) <- c(n_age, n_vacc, n_risk)
dim(E) <- c(n_age, n_vacc, n_risk)
dim(I) <- c(n_age, n_vacc, n_risk)
dim(R) <- c(n_age, n_vacc, n_risk)
dim(Is) <- c(n_age, n_vacc, n_risk)
dim(Rc) <- c(n_age, n_vacc, n_risk)
dim(N0) <- c(n_age, n_vacc, n_risk)

#Dimensions for migration
dim(tt_migration) <- no_migration_changes
dim(migration_distribution) <- c(6, n_age, n_vacc, n_risk)
dim(migration_in_number) <- c(no_migration_changes, n_age, n_vacc, n_risk)
dim(migration_distribution_values) <- c(no_migration_changes, 6, n_age, n_vacc, n_risk)
dim(migration) <- c(n_age, n_vacc, n_risk)
dim(migration_adjusted) <- c(n_age, n_vacc, n_risk)
dim(migration_S) <- c(n_age, n_vacc, n_risk)
dim(migration_E) <- c(n_age, n_vacc, n_risk)
dim(migration_I) <- c(n_age, n_vacc, n_risk)
dim(migration_R) <- c(n_age, n_vacc, n_risk)
dim(migration_Is) <- c(n_age, n_vacc, n_risk)
dim(migration_Rc) <- c(n_age, n_vacc, n_risk)
