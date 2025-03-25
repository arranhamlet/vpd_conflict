#' Process Dust Model Data
#'
#' This helper function processes model output data by transforming it into a structured data frame format for analysis.
#'
#' @param this_obj The model data object to be processed.
#' @param present_dimensions Vector of dimension names associated with the data object.
#' @param colnames Column names for the melted data frame.
#' @param time_length The total length of the time dimension.
#' @param x Index corresponding to the state being processed.
#' @param model_system The model system object containing particle information.
#' @param dimension_names List of dimension names for states, time, and other variables.
#' @param dust_state List containing unpacked model state data.
#'
#' @return A list containing two data frames: one with fully melted data, and one aggregated by state, time, and run.
#' @export
process_obj <- function(this_obj, present_dimensions, colnames, time_length, x, model_system, dimension_names, dust_state) {
  
  #' If the model has multiple particles or the object dimensions mismatch the expected dimensions
  if (model_system$n_particles > 1 || length(dim(this_obj)) != length(present_dimensions)) {
    
    #' Identify the particle dimension index
    this_is_particles <- last(which(dim(this_obj) == length(time_length))) - 1
    
    #' Identify dimension names excluding the particle dimension
    these_names <- setdiff(1:length(dim(this_obj)), this_is_particles)
    
    #' Assign appropriate dimension names for non-particle dimensions
    lapply(these_names, function(i) {
      dimnames(this_obj)[[i]] <- dimension_names[[which(names(dimension_names) == present_dimensions[these_names == i])]][[1]]
    })
    
    #' Assign particle run names
    dimnames(this_obj)[[this_is_particles]] <- paste0("run_", 1:model_system$n_particles)
    
    #' Melt the array to long format with appropriate column structure
    melted_array <- reshape2::melt(this_obj) %>%
      setNames(c(colnames[1:(which(colnames == "time") - 1)], "run", "time", "value")) %>%
      mutate(state = names(dust_state)[x]) %>%
      select(time, state, all_of(colnames[1:(which(colnames == "time") - 1)]), "run", "value")
    
    #' Create an aggregated array by grouping and summing values
    aggregate_array <- melted_array %>%
      fgroup_by(state, time, run) %>%
      fsummarise(
        value = sum(value)
      )
    
    list(melted_array, aggregate_array)
    
  } else {
    
    #' Melt data directly for single-particle models
    melted_array <- melt(this_obj) %>%
      setNames(c(which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]], "value")) %>%
      mutate(state = names(dust_state)[x]) %>%
      select(time, state, everything(), value)
    
    #' Create an aggregated array by grouping and summing values
    aggregate_array <- melted_array %>%
      fgroup_by(state, time) %>%
      fsummarise(
        value = sum(value)
      )
    
    list(melted_array, aggregate_array)
  }
}