check_parameter <- function(parameter, 
                            check_integer = FALSE, 
                            check_probability = FALSE, 
                            check_above_zero = FALSE,
                            check_non_negative = FALSE,
                            check_not_above_reference_value = NULL,
                            check_if_sum_above_zero = FALSE) {
  parameter_name <- deparse(substitute(parameter))
  
  if (is.list(parameter)) {
    parameter_names <- names(parameter)
    parameter <- unlist(parameter)
  } else {
    parameter_names <- as.character(seq_along(parameter))
  }
  
  if (!is.numeric(parameter)) {
    warning(paste("The parameter", parameter_name, "must be numeric."))
    return(rep(FALSE, length(parameter)))
  }
  
  failed_conditions <- vector("list", length(parameter))
  failure_table <- data.frame(Parameter = character(), Value = numeric(), Reason = character())
  
  if (check_if_sum_above_zero && sum(parameter) <= 0) {
    failure_table <- rbind(failure_table, data.frame(Parameter = "Sum of values", Value = sum(parameter), Reason = "Sum must be above zero"))
  }
  
  for (i in seq_along(parameter)) {
    errors <- c()
    if (check_integer && parameter[i] != as.integer(parameter[i])) {
      errors <- c(errors, "must be an integer")
    }
    
    if (check_above_zero && parameter[i] <= 0) {
      errors <- c(errors, "must be above 0")
    }
    
    if (check_non_negative && parameter[i] < 0) {
      errors <- c(errors, "must be non-negative")
    }
    
    if (check_probability && (parameter[i] < 0 || parameter[i] > 1)) {
      errors <- c(errors, "must be a probability (between 0 and 1)")
    }
    
    if (!is.null(check_not_above_reference_value) && parameter[i] > check_not_above_reference_value) {
      errors <- c(errors, paste("must be not above", check_not_above_reference_value))
    }
    
    if (length(errors) > 0) {
      param_label <- ifelse(!is.null(parameter_names[i]), parameter_names[i], parameter[i])
      failed_conditions[[i]] <- paste("The parameter", param_label, "must be", paste(errors, collapse = " and "))
      failure_table <- rbind(failure_table, data.frame(Parameter = param_label, Value = parameter[i], Reason = paste(errors, collapse = " and ")))
    }
  }
  
  return(failure_table)
}
