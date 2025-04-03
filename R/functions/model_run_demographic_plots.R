
model_run_demographic_plots <- function(
    clean_model_df,
    demog_data,
    end_year = NA
    ){
  
  #Update end year
  clean_model_df <-   clean_model_df %>% filter(run == "run_1")
    # fgroup_by(state, time, age, vaccination, risk) %>%
    # fsummarise(
    #   value = median(value),
    #   value_low = quantile(value, 0.025),
    #   value_high = quantile(value, 0.975)
    # )

  end_year <- if(is.na(end_year)) max(demog_data$years) else end_year
  
  #Subset to end year
  full_time_year <- data.frame(model_time = 1:max(clean_model_df$time),
                               year = demog_data$years)
  
  model_data_subset <- clean_model_df %>%
    filter(time <= full_time_year %>% filter(year == end_year) %>% pull(model_time))
  
  total_population_df <- data.frame(year = demog_data$years, value = 1000 * rowSums(demog_data$population_data)) %>%
    filter(year <= end_year)
  
  #Five year groupings
  age_groups <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
    "90-94", "95-99", "100+"
  )
  
  #Plot linegraph
  linegraph <- ggplot() +
    geom_line(
      data = subset(model_data_subset, state == "total_pop"),
      mapping = aes(
        x = min(demog_data$years) - 1 + time,
        y = value,
        color = "Model"
      )) +
    geom_line(
      data = total_population_df,
      mapping = aes(
        x = year,
        y = value, 
        color = "UN WPP"
      )
    ) + 
    scale_y_continuous(label = scales::comma) +
    labs(x = "Year",
         y = "Population",
         color = "",
         subtitle = paste0(paste0("Model population estimated in ", end_year, ": ", formatC(subset(model_data_subset, time == full_time_year %>% filter(year == end_year) %>% pull(model_time) & state == "total_pop")$value, big.mark = ",", format = "fg")), "\n",
                           paste0("UN WPP population estimated in ", end_year, ": ", formatC(sum(total_population_df[nrow(total_population_df), ]$value), big.mark = ",", format = "fg")))) +
    theme_bw()
  
  #Now create age pyramid
  #Plot histogram
  age_pyramid_df <- rbind(
    
    data.frame(age = 1:101,
               value = subset(clean_df,state == "S" & time == full_time_year %>% filter(year == end_year) %>% pull(model_time) & age != "All") %>% mutate(age = as.numeric(age)) %>% pull(value),
               type = "Model estimate"),
    
    data.frame(age = 1:101,
               value = as.numeric(demog_data$population_data[full_time_year %>% filter(year == end_year) %>% pull(model_time), ] * 1000),
               type = "UN WPP")
    
  ) %>% 
    group_by(type, ceiling(age/5)) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    set_names("type", "age_group", "value") %>%
    mutate(age_group = factor(c(age_groups, age_groups), levels = age_groups)) %>%
    group_by(type) %>%
    mutate(age_group_prop = value/sum(value))
  
  #Create age pyramid 
  pop_pyramid_absolute <- ggplot(mapping = aes(
    x = age_group,
    fill = type
  )) +
    scale_y_continuous(label = scales::comma, limits = c(-max(abs(age_pyramid_df$value)), max(abs(age_pyramid_df$value)))) +
    geom_bar(
      data = subset(age_pyramid_df, type == "Model estimate"),
      mapping = aes( y = value),
      stat = "identity"
    ) +
    geom_bar(
      data = subset(age_pyramid_df, type == "UN WPP"),
      mapping = aes(y = -1 * value),
      stat = "identity"
    ) +
    coord_flip() +
    theme_bw() +
    labs(title = paste0("Population age pyramid (", end_year, ")"),
         y = "",
         x = "",
         fill = "") +
    theme(legend.position = "bottom")
  
  
  pop_pyramid_prop <- ggplot(mapping = aes(
    x = age_group,
    fill = type
  )) +
    geom_bar(
      data = age_pyramid_df,
      mapping = aes( y = age_group_prop),
      stat = "identity",
      position = position_dodge()
    ) +
    theme_bw() +
    labs(title = paste0("Population proportion by age group (", end_year, ")"), 
         y = "",
         x = "",
         fill = "") +
    theme(legend.position = "bottom")
  
  #Export
  list(
    population_linegraph = linegraph,
    pop_pyramid_absolute = pop_pyramid_absolute,
    pop_pyramid_prop = pop_pyramid_prop,
    pyramid_data = age_pyramid_df
    )
  
}