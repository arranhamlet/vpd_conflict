calculate_foi_from_R0 <- function(R0, contact_matrix, S, infectious_period = 5) {
  # Arguments:
  # R0              = basic reproduction number (scalar)
  # contact_matrix  = age-by-age contact matrix (matrix)
  # S               = vector of susceptibles per age group
  # infectious_period = average infectious period in days (default 5)
  
  # Step 1: Construct unscaled Next Generation Matrix (NGM)
  # Assume unit infectivity and full susceptibility
  unscaled_NGM <- contact_matrix
  
  # Step 2: Get dominant eigenvalue and eigenvector
  eig <- eigen(unscaled_NGM)
  lambda_max <- Re(eig$values[1])
  v <- Re(eig$vectors[, 1])
  
  # Step 3: Scale contact matrix to match R0
  scaling_factor <- R0 / lambda_max
  beta <- scaling_factor / infectious_period  # Transmission rate per contact per day
  
  # Step 4: Estimate relative infectious distribution (normalize eigenvector)
  I_rel <- v / sum(v)  # Relative I per age group
  N <- rep(1, length(S))  # Assume equal population if unknown; cancels out
  I_over_N <- I_rel / N
  
  # Step 5: Calculate FOI
  FOI <- beta * (contact_matrix %*% I_over_N)
  names(FOI) <- paste0("age_group_", seq_along(FOI))
  
  return(as.vector(FOI))
}
