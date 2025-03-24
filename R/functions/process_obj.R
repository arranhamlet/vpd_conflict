# Helper function for data transformation
process_obj <- function(this_obj, present_dimensions, colnames, time_length, x, model_system, dimension_names, dust_state) {
  if (model_system$n_particles > 1 || length(dim(this_obj)) != length(present_dimensions)) {
    this_is_particles <- last(which(dim(this_obj) == length(time_length))) - 1
    these_names <- setdiff(1:length(dim(this_obj)), this_is_particles)
    
    lapply(these_names, function(i) {
      dimnames(this_obj)[[i]] <- dimension_names[[which(names(dimension_names) == present_dimensions[these_names == i])]][[1]]
    })
    
    dimnames(this_obj)[[this_is_particles]] <- paste0("run_", 1:model_system$n_particles)
    melted_array <- reshape2::melt(this_obj) %>%
      setNames(c(colnames[1:(which(colnames == "time") - 1)], "run", "time", "value")) %>%
      mutate(state = names(dust_state)[x]) %>%
      select(time, state, all_of(colnames[1:(which(colnames == "time") - 1)]), "run", "value")
    
    #Speedy sum
    aggregate_array <- melted_array %>%
      fgroup_by(state, time, run) %>%
      fsummarise(
        value = sum(value)
      )

    list(melted_array, aggregate_array)
    
  } else {
    
    melted_array <- melt(this_obj) %>%
      setNames(c(which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]], "value")) %>%
      mutate(state = names(dust_state)[x]) %>%
      select(time, state, everything(), value)

    #Speedy sum
    aggregate_array <- melted_array %>%
      fgroup_by(state, time) %>%
      fsummarise(
        value = sum(value)
      )
    
    list(melted_array, aggregate_array)
  }
}