
unpack_dust2 <- function(model_system, model_object, dimension_names, which_state_dimensions){
  
  #Unpack state to lists
  dust_state <- dust_unpack_state(model_system, model_object)
  
  #Work out which are compartments and which are not
  these_are_compartments <- sapply(model_system$packing_state, function(x) length(x) != 0)
  
  #Time length
  time_length <- dimension_names$time[[1]]
  
  #Unpack all the compartments
  unpacked_compartments <- do.call(plyr::rbind.fill, sapply(as.numeric(which(these_are_compartments)), function(x){
    
    #Unpack this object
    this_obj <- dust_state[[x]]
    
    #Identify time
    this_is_time <- which(dim(this_obj) == length(dimension_names[[which(names(dimension_names) == "time")]][[1]]))
    
    #These are dimensions
    present_dimensions <- which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]]

    #Name things correctly based on if there are multiple runs
    these_names <- if(model_system$n_particles > 1 | length(dim(this_obj)) != length(present_dimensions)){
      this_is_particles <- last(which(dim(this_obj) == length(time_length))) - 1
      (1:length(dim(this_obj)))[-this_is_particles]
    } else {
      1:length(present_dimensions)
    }
    
    #Loop through dimnames
    for(i in these_names){
      dimnames(this_obj)[[i]] <- dimension_names[[which(names(dimension_names) == present_dimensions[these_names == i])]][[1]]
    }
    
    #Change names based on whats going on
    if(model_system$n_particles > 1 | length(dim(this_obj)) != length(present_dimensions)){
      
      dimnames(this_obj)[[this_is_particles]] <- paste0("run_", 1:model_system$n_particles)
      
      colnames <- which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]]
      
      
      melted_array <- reshape2::melt(this_obj) %>%
        setNames(c(colnames[1:(which(colnames == "time") - 1)], "run", "time", "value")) %>%
        mutate(state = names(dust_state)[x]) %>%
        select(time, state, colnames[1:(which(colnames == "time") - 1)], "run", "value")
      
      aggregate_array <- melted_array %>%
        group_by(state, time, run) %>%
        summarise(value = sum(value)) %>%
        as.data.frame()
      
    } else {
      
      melted_array <- reshape2::melt(this_obj) %>%
        set_names(c(which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]], "value")) %>%
        mutate(state = names(dust_state)[x]) %>%
        select(time, state, which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]], value)
      
      aggregate_array <- melted_array %>%
        group_by(state, time) %>%
        summarise(value = sum(value)) %>%
        as.data.frame()
      
    }
    
    combo_df <- plyr::rbind.fill(melted_array, aggregate_array) %>%
      mutate(across(names(melted_array)[-which(names(melted_array) %in% names(aggregate_array))], as.character)) %>%
      mutate(across(names(melted_array)[-which(names(melted_array) %in% names(aggregate_array))], fct_na_value_to_level, level = "All"))
    
    combo_df

  }, simplify = FALSE))
  
  #Add in the non-compartment outputs
  unpacked_noncompartments <- data.table::rbindlist(sapply(as.numeric(which(!these_are_compartments)), function(x){
    
    #Unpack this object
    this_obj <- dust_state[[x]]
    
    if(model_system$n_particles > 1){
      
      this_obj %>%
        t %>%
        as.data.frame %>%
        gather(key = run,
               value = value) %>%
        mutate(time = rep(dimension_names[[which(names(dimension_names) == "time")]][[1]], model_system$n_particles),
               state = names(dust_state)[x],
               run = gsub("V", "run_", run))
      
    } else {
      
      #Loop through dimnames
      data.frame(this_obj) %>% 
        set_names("value") %>%
        mutate(time = dimension_names[[which(names(dimension_names) == "time")]][[1]]) %>%
        mutate(state = names(dust_state)[x])
      
    }

  }, simplify = FALSE))
  
  #Combine and spit out
  plyr::rbind.fill(
    unpacked_compartments, 
    unpacked_noncompartments
    ) %>%
    mutate(across(unique(unlist(which_state_dimensions)), replace_na, "All")) %>%
    mutate(state = factor(state, levels = c(names(these_are_compartments[these_are_compartments == T]), names(these_are_compartments[these_are_compartments == F]))))
    
}
