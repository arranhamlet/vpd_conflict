unpack_dust2 <- function(model_system, model_object, dimension_names, which_state_dimensions) {
  
  dust_state <- dust_unpack_state(model_system, model_object)
  
  these_are_compartments <- sapply(model_system$packing_state, length) != 0
  time_length <- dimension_names$time[[1]]
  
  unpacked_compartments <- rbindlist(
    lapply(as.numeric(which(these_are_compartments)), function(x) {
      this_obj <- dust_state[[x]]
      present_dimensions <- which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]]
      colnames <- present_dimensions
      processed_data <- process_obj(this_obj, present_dimensions, colnames, time_length, x, model_system, dimension_names, dust_state)
      rbindlist(processed_data, fill = T)
    }), fill = TRUE
  )
  
  unpacked_noncompartments <- rbindlist(
    lapply(as.numeric(which(!these_are_compartments)), function(x) {
      this_obj <- dust_state[[x]]
      
      if (model_system$n_particles > 1) {
        as.data.frame(t(this_obj)) %>%
          gather(key = "run", value = "value") %>%
          mutate(time = rep(time_length, model_system$n_particles),
                 state = names(dust_state)[x],
                 run = gsub("V", "run_", run))
      } else {
        data.frame(this_obj) %>%
          setNames("value") %>%
          mutate(time = time_length, state = names(dust_state)[x])
      }
    }), fill = TRUE
  )
  
  # Combine and return
  colnames_use <- unique(unlist(which_state_dimensions))
  colnames_use <- colnames_use[-which(colnames_use == "time")]
  
  bind_rows(unpacked_compartments, unpacked_noncompartments) %>%
    mutate(across(colnames_use, as.character)) %>%
    mutate(across(c(time, value), as.numeric)) %>%
    mutate(across(colnames_use, replace_na, "All")) %>%
    mutate(state = factor(state, levels = c(
      names(these_are_compartments[these_are_compartments]),
      names(these_are_compartments[!these_are_compartments])
    )))
  
}