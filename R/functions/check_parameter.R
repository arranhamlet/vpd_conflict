#' Parameter Validation Function
#'
#' This function checks the validity of a given parameter based on a series of logical conditions. 
#' It supports conditions such as being an integer, a probability, non-negative, or not exceeding a reference value.
#'
#' @param parameter The parameter(s) to check (numeric vector or list)
#' @param check_integer Logical; if TRUE, ensures the parameter is an integer.
#' @param check_probability Logical; if TRUE, ensures the parameter is a probability (between 0 and 1).
#' @param check_above_zero Logical; if TRUE, ensures the parameter is strictly above zero.
#' @param check_non_negative Logical; if TRUE, ensures the parameter is non-negative.
#' @param check_not_above_reference_value Numeric; if provided, ensures the parameter is not greater than this value.
#' @param check_if_sum_above_zero Logical; if TRUE, ensures the sum of the parameter's values is greater than zero.
#'
#' @return A data frame listing any parameter values that failed their respective checks, along with the reason for failure.
#' @export
check_parameter <- function(parameter, 
                            check_integer = FALSE, 
                            check_probability = FALSE, 
                            check_above_zero = FALSE,
                            check_non_negative = FALSE,
                            check_not_above_reference_value = NULL,
                            check_if_sum_above_zero = FALSE) {
  #' Capture the parameter's name for better warning messages
  parameter_name <- deparse(substitute(parameter))
  
  #' Handle parameter names if provided as a list
  if (is.list(parameter)) {
    parameter_names <- names(parameter)
    parameter <- unlist(parameter)
  } else {
    parameter_names <- as.character(seq_along(parameter))
  }
  
  #' Check if parameter is numeric
  if (!is.numeric(parameter)) {
    warning(paste("The parameter", parameter_name, "must be numeric."))
    return(rep(FALSE, length(parameter)))
  }
  
  #' Create an empty list to track failures
  failed_conditions <- vector("list", length(parameter))
  failure_table <- data.frame(Parameter = character(), Value = numeric(), Reason = character())
  
  #' Check if the sum of values exceeds zero
  if (check_if_sum_above_zero && sum(parameter) <= 0) {
    failure_table <- rbind(failure_table, data.frame(Parameter = "Sum of values", Value = sum(parameter), Reason = "Sum must be above zero"))
  }
  
  #' Loop through each parameter entry and validate conditions
  for (i in seq_along(parameter)) {
    errors <- c()
    
    #' Ensure parameter is an integer if specified
    if (check_integer && parameter[i] != as.integer(parameter[i])) {
      errors <- c(errors, "must be an integer")
    }
    
    #' Ensure parameter is strictly above zero if specified
    if (check_above_zero && parameter[i] <= 0) {
      errors <- c(errors, "must be above 0")
    }
    
    #' Ensure parameter is non-negative if specified
    if (check_non_negative && parameter[i] < 0) {
      errors <- c(errors, "must be non-negative")
    }
    
    #' Ensure parameter is a valid probability if specified
    if (check_probability && (parameter[i] < 0 || parameter[i] > 1)) {
      errors <- c(errors, "must be a probability (between 0 and 1)")
    }
    
    #' Ensure parameter is not greater than the reference value if specified
    if (!is.null(check_not_above_reference_value) && parameter[i] > check_not_above_reference_value) {
      errors <- c(errors, paste("must be not above", check_not_above_reference_value))
    }
    
    #' Log any errors identified for this entry
    if (length(errors) > 0) {
      param_label <- ifelse(!is.null(parameter_names[i]), parameter_names[i], parameter[i])
      failed_conditions[[i]] <- paste("The parameter", param_label, "must be", paste(errors, collapse = " and "))
      failure_table <- rbind(failure_table, data.frame(Parameter = param_label, Value = parameter[i], Reason = paste(errors, collapse = " and ")))
    }
  }
  
  #' Return the collected failures (empty if all checks pass)
  return(failure_table)
}
