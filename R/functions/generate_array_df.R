#' Generate a Long-Format Data Frame Representing a Multi-Dimensional Array
#'
#' This function generates a tidy `data.frame` representing a multi-dimensional array.
#' Each row corresponds to a unique combination of indices, along with a `value` column.
#' Optionally, specific entries can be updated using a provided `updates` data.frame.
#'
#' @param dim1 Integer. Size of the first dimension (required).
#' @param dim2 Integer. Size of the second dimension (optional).
#' @param dim3 Integer. Size of the third dimension (optional).
#' @param dim4 Integer. Size of the fourth dimension (optional).
#' @param updates Optional `data.frame` with columns `dim1`, `dim2`, ..., and `value`.
#'   Specifies specific array elements to update from 0 to a given value.
#'   Only the dimensions provided in `updates` will be used.
#'
#' @return A `data.frame` with the following columns:
#' \describe{
#'   \item{ID}{Row ID, uniquely identifying each entry.}
#'   \item{dim1...dimN}{Index positions for each dimension.}
#'   \item{value}{The value assigned to that array position (default 0 unless updated).}
#' }
#'
#' @examples
#' # Create a 3x1x1x3 array and update one value
#' updates <- data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 0.5)
#' df <- generate_array_df(3, 1, 1, 3, updates = updates)
#' head(df)
#'
#' @export
generate_array_df <- function(dim1, dim2 = NULL, dim3 = NULL, dim4 = NULL, default_value = 0, updates = NULL) {
  # Combine non-null dimensions
  dims <- c(dim1, dim2, dim3, dim4)
  dims <- dims[!sapply(dims, is.null)]
  n_dims <- length(dims)
  
  # Generate all possible index combinations
  index_grid <- as.data.frame(do.call(expand.grid, c(lapply(dims, seq_len), KEEP.OUT.ATTRS = FALSE)))
  names(index_grid) <- paste0("dim", seq_len(n_dims))
  
  # Initialize values to 0
  index_grid$value <- default_value
  
  # Apply updates if provided
  if (!is.null(updates)) {
    # Check required columns exist
    required_cols <- c(paste0("dim", seq_len(n_dims)), "value")
    if (!all(required_cols %in% names(updates))) {
      stop("Updates data.frame must contain: ", paste(required_cols, collapse = ", "))
    }
    
    # Match and update
    for (row in seq_len(nrow(updates))) {
      condition <- rep(TRUE, nrow(index_grid))
      for (d in seq_len(n_dims)) {
        condition <- condition & (index_grid[[paste0("dim", d)]] == updates[[paste0("dim", d)]][row])
      }
      index_grid$value[condition] <- updates$value[row]
    }
  }
  
  # Add ID column
  index_grid <- cbind(ID = seq_len(nrow(index_grid)), index_grid)
  
  index_grid
  
}
