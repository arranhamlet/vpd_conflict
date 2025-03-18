
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
    
    #Name things correctly based on if there are multiple runs
    these_names <- if(model_system$n_particles > 1){
      this_is_particles <- last(which(dim(this_obj) == length(time_length))) - 1
      (1:length(dim(this_obj)))[-this_is_time]
    } else {
      1:length(which_state_dimensions[[x]])
    }
    
    #Loop through dimnames
    for(i in these_names){
      dimnames(this_obj)[[i]] <- unlist(dimension_names[[which_state_dimensions[[x]][which(these_names == i)]]])
    }
    
    #Change names based on whats going on
    if(model_system$n_particles > 1){
      
      dimnames(this_obj)[[this_is_particles]] <- paste0("run_", 1:model_system$n_particles)
      
      melted_array <- reshape2::melt(this_obj) %>%
        set_names(c(c(which_state_dimensions[[x]][1:(length(which_state_dimensions[[x]])-1)], "run", which_state_dimensions[[x]][length(which_state_dimensions[[x]])]), "value")) %>%
        mutate(state = names(dust_state)[x]) %>%
        select(time, state, c(which_state_dimensions[[x]][1:(length(which_state_dimensions[[x]])-1)], "run", which_state_dimensions[[x]][length(which_state_dimensions[[x]])]), value)
      
      aggregate_array <- melted_array %>%
        group_by(state, time, run) %>%
        summarise(value = sum(value)) %>%
        as.data.frame()
      
    } else {
      
      melted_array <- reshape2::melt(this_obj) %>%
        set_names(c(which_state_dimensions[[x]], "value")) %>%
        mutate(state = names(dust_state)[x]) %>%
        select(time, state, which_state_dimensions[[x]], value)
      
      aggregate_array <- melted_array %>%
        group_by(state, time) %>%
        summarise(value = sum(value)) %>%
        as.data.frame()
      
    }
    
    combo_df <- plyr::rbind.fill(melted_array, aggregate_array) %>%
      mutate(across(names(melted_array)[-which(names(melted_array) %in% names(aggregate_array))], fct_na_value_to_level, level = "All"))
    
    combo_df

  }, simplify = FALSE))
  
  #Add in the non-compartment outputs
  unpacked_noncompartments <- data.table::rbindlist(sapply(as.numeric(which(!these_are_compartments)), function(x){
    
    #Unpack this object
    this_obj <- dust_state[[x]]
    
    #Loop through dimnames
    data.frame(this_obj) %>% 
      set_names("value") %>%
      mutate(time = dimension_names[[which(names(dimension_names) == "time")]][[1]]) %>%
      mutate(state = names(dust_state)[x])
    
  }, simplify = FALSE))
  
  #Combine and spit out
  plyr::rbind.fill(
    unpacked_compartments, 
    unpacked_noncompartments
    ) %>%
    mutate(state = factor(state, levels = names(dust_state)))
  
}
