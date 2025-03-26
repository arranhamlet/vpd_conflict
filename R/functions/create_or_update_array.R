create_or_update_array <- function(dim1, dim2 = NULL, dim3 = NULL, dim4 = NULL,
                                   updates = NULL, initial_value = 0) {
  dims <- c(dim1, dim2, dim3, dim4)
  dims <- dims[!sapply(dims, is.null)]
  arr <- array(initial_value, dim = dims)
  
  if (is.null(updates)) return(arr)
  
  updates <- as.data.frame(updates)
  
  for (i in seq_len(nrow(updates))) {
    # Build full index vector; default to 1 for missing dimensions
    full_idx <- lapply(seq_along(dims), function(d) {
      dim_name <- paste0("dim", d)
      if (dim_name %in% names(updates)) updates[[dim_name]][i] else seq_len(dims[d])
    })
    
    # Expand across unspecified dimensions
    grid <- expand.grid(full_idx)
    
    # Translate human index to R's internal index and assign value
    for (j in seq_len(nrow(grid))) {
      # Convert multidimensional index to linear index
      idx <- as.integer(grid[j, ])
      linear_index <- 1 + sum((idx - 1) * cumprod(c(1, dims[-length(dims)])))
      arr[linear_index] <- updates$value[i]
    }
  }
  
  return(arr)
}
