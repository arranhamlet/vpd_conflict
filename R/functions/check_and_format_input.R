#' Check and Format Input Data
#'
#' This function ensures input data matches the required dimensions, handling various cases such as zero dimensions,
#' single-value expansion, and data that is either too short or too long.
#'
#' @param input The input data to be formatted (vector, matrix, or array).
#' @param dim1 Required size for the first dimension.
#' @param dim2 Required size for the second dimension (default = NA).
#' @param dim3 Required size for the third dimension (default = NA).
#' @param dim4 Required size for the fourth dimension (default = NA).
#'
#' @return The input data reformatted as a vector, matrix, or array with the specified dimensions.
#' @export
check_and_format_input <- function(input, dim1, dim2 = NA, dim3 = NA, dim4 = NA) {
  
  #' Collect required dimensions into a vector
  required_dims <- c(dim1, dim2, dim3, dim4)
  
  #' Identify non-NA dimensions
  non_na_dims <- required_dims[!is.na(required_dims)]
  
  #' Count zero dimensions (special handling required)
  zero_count <- sum(non_na_dims == 0)
  
  #' Special handling for cases where 3 dimensions are zero
  if (zero_count == 3) {
    required_size <- max(non_na_dims)
    input <- rep(input, length.out = required_size)
    as.vector(input)  # Return as a vector
    
    #' Special handling for cases where 2 dimensions are zero
  } else if (zero_count == 2) {
    required_size <- prod(non_na_dims, na.rm = TRUE)
    input <- rep(input, length.out = required_size)
    matrix(input, nrow = non_na_dims[1], ncol = non_na_dims[2])  # Return as a matrix
    
    #' Standard case where no special handling is required
  } else {
    required_size <- prod(non_na_dims, na.rm = TRUE)
    
    #' Handle single-value input by repeating it to fit required size
    if (length(input) == 1) {
      input <- rep(input, required_size)
      
      #' Handle input that's too short by repeating elements
    } else if (length(input) < required_size) {
      input <- rep(input, length.out = required_size)
      
      #' Handle input that's too long by reducing to the median (or appropriate summary)
    } else if (length(input) > required_size) {
      input <- rep(median(input), required_size)
    }
    
    #' Return as an array with appropriate dimensions
    array(input, dim = non_na_dims)
  }
}