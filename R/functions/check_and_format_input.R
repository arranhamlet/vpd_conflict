check_and_format_input <- function(input, dim1, dim2 = NA, dim3 = NA, dim4 = NA) {
  # Determine the required dimensions
  required_dims <- c(dim1, dim2, dim3, dim4)

  # Special handling for zero dimensions
  non_na_dims <- required_dims[!is.na(required_dims)]
  zero_count <- sum(non_na_dims == 0)

  if (zero_count == 3) {
    required_size <- max(non_na_dims)
    input <- rep(input, length.out = required_size)
    as.vector(input)
  } else if (zero_count == 2) {
    required_size <- prod(non_na_dims, na.rm = TRUE)
    input <- rep(input, length.out = required_size)
    matrix(input, nrow = non_na_dims[1], ncol = non_na_dims[2])
  } else {
    # Regular size calculation for arrays
    required_size <- prod(non_na_dims, na.rm = TRUE)

    # Handle single-value input
    if (length(input) == 1) {
      input <- rep(input, required_size)
    } else if (length(input) < required_size) {
      # Handle input that's too short by repeating elements
      input <- rep(input, length.out = required_size)
    } else if (length(input) > required_size) {
      # Handle input that's too long by reducing with median (or appropriate summary)
      input <- rep(median(input), required_size)
    }
    array(input, dim = non_na_dims)
  }
}

