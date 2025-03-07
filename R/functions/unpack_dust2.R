
unpack_dust2 <- function(model_system, model_object, dimension_names){
  
  #Unpack state to lists
  dust_state <- dust_unpack_state(model_system, model_object)
  
  #Work out which are compartments and which are not
  these_are_compartments <- sapply(model_system$packing_state, function(x) length(x) != 0)
  
  #Unpack all the compartments
  unpacked_compartments <- data.table::rbindlist(sapply(as.numeric(which(these_are_compartments)), function(x){
    
    print(x)
    
    #Unpack this object
    this_obj <- dust_state[[x]]
    
    #Loop through dimnames
    for(i in 1:length(dimension_names)){
      dimnames(this_obj)[[i]] <- unlist(dimension_names[[i]])
    }
    
    melted_array <- reshape2::melt(this_obj) %>%
      set_names(c(names(dimension_names), "value")) %>%
      mutate(state = names(dust_state)[x]) %>%
      select(time, state, names(dimension_names), value)
    
    melted_array
    
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
