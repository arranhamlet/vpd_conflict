#' Convert a Long-Format Data Frame to a Multi-Dimensional Array
#'
#' This function takes a tidy `data.frame` representation of an array
#' (typically generated by [generate_array_df()]) and converts it back
#' into a multi-dimensional array, using the `dim1`, `dim2`, etc. columns
#' to define the index positions and a `value` column to populate the array.
#'
#' @param df A `data.frame` containing columns named `dim1`, `dim2`, ..., and `value`.
#'   Each row represents a position in the array and its corresponding value.
#'   Only the `dim` columns and `value` are required; `ID` and other columns are ignored.
#'
#' @return A multi-dimensional `array` reconstructed from the data frame.
#'
#' @examples
#' # Generate a 3x1x1x3 array and convert it back from a data.frame
#' updates <- data.frame(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 2, value = 0.5)
#' df <- generate_array_df(3, 1, 1, 3, updates = updates)
#' arr <- df_to_array(df)
#' arr[1,1,1,2]  # Should be 0.5
#'
#' @seealso [generate_array_df()]
#' @export
# df_to_array <- function(df) {
#   dim_cols <- grep("^dim\\d+$", names(df), value = TRUE)
#   n_dims <- length(dim_cols)
#   dim_cols <- paste0("dim", seq_len(n_dims))
#   dims <- sapply(dim_cols, function(col) max(df[[col]]))
# 
#   flat <- numeric(prod(dims))
# 
#   for (row in seq_len(nrow(df))) {
#     idx <- as.integer(df[row, dim_cols])
#     rev_idx <- rev(idx)
#     strides <- c(1, cumprod(rev(dims))[-length(dims)])
#     lin_index <- sum((rev_idx - 1) * strides) + 1
#     flat[lin_index] <- df$value[row]
#   }
# 
#   array(flat, dim = dims)
# }
# 
# # 


df_to_array <- function(df, version = "new") {

  if(version == "new"){
    dim_cols <- grep("^dim\\d+$", names(df), value = TRUE)
    n_dims <- length(dim_cols)
    dim_cols <- paste0("dim", seq_len(n_dims))
    dims <- sapply(dim_cols, function(col) max(df[[col]]))
    
    create_array <- array(0, dims)
    
    for (row in seq_len(nrow(df))) {
      idx <- as.integer(df[row, dim_cols])
      idx_list <- as.list(idx)
      call <- as.call(c(list(as.name("["), quote(create_array)), idx_list))
      assign_call <- call("<-", call, df$value[row])
      eval(assign_call)
    }
    
    create_array
  } else {
      dim_cols <- grep("^dim\\d+$", names(df), value = TRUE)
      n_dims <- length(dim_cols)
      dim_cols <- paste0("dim", seq_len(n_dims))
      dims <- sapply(dim_cols, function(col) max(df[[col]]))

      flat <- numeric(prod(dims))

      for (row in seq_len(nrow(df))) {
        idx <- as.integer(df[row, dim_cols])
        rev_idx <- rev(idx)
        strides <- c(1, cumprod(rev(dims))[-length(dims)])
        lin_index <- sum((rev_idx - 1) * strides) + 1
        flat[lin_index] <- df$value[row]
      }

      array(flat, dim = dims)
  }
  
}
