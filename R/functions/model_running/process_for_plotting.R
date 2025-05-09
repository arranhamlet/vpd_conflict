
process_for_plotting <- function(run_model_output, input_data){
  
  #Aggregate across runs
  aggregate_df <- run_model_output %>%
    subset(state %in% c("S", "E", "I", "R", "Is", "Rc", "new_case", "total_pop", "Reff")) %>%
    mutate(year = floor(input_data$year_start + (time * input_data$time_adjust)/365)) %>%
    fgroup_by(state, age, vaccination, risk, year) %>%
    fsummarise(value = median(value),
               value_min = quantile(value, 0.025),
               value_max = quantile(value, 0.975))

  #Aggregate by susceptibility
  susceptibility_data <- subset(aggregate_df, state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All") %>%
    mutate(
      status = case_when(
        state == "S" & vaccination == 1 ~ "Susceptible",
        state == "S" & vaccination > 1 ~ "Vaccine protected",
        state != "S" & vaccination == 1 ~ "Exposure protected",
        state != "S" & vaccination > 1 ~ "Vaccine and exposure protected"
        
      ),
      status = factor(status, levels = c("Susceptible", "Vaccine protected", "Exposure protected", 
                                         "Vaccine and exposure protected"))
    ) %>%
    group_by(
      year, age, status
    ) %>%
    summarise(
      value = sum(value),
      .groups = "keep"
    ) %>%
    mutate(
      coverage = value/sum(value, na.rm = T),
      coverage = case_when(
        is.nan(coverage) ~ 0,
        !is.nan(coverage) ~ coverage
      )
    )
  
  #Plots
  #All states
  #Plot
  ggplot(
    data = clean_df %>%
      filter(age == "All" & time > 5),
    mapping = aes(
      x = time + year_start,
      y = value
    )
  ) +
    geom_bar(stat = "identity") +
    labs(
      x = "Year",
      y = "Cases"
    ) +
    scale_y_continuous(label = scales::comma) +
    theme_bw() +
    facet_wrap(~state, scales = "free_y")
  
  #By age
  vaccine_by_age <- ggplot(
    data = vacc_age %>%
      subset(time == max(time) &
               age != "All") %>%
      mutate(vaccination = case_when(
        vaccination == 1 ~ "Unvaccinated",
        vaccination %in% 2:3 ~ "1 dose",
        vaccination %in% 4:5 ~ "2 doses",
      ),
      vaccination = factor(vaccination, levels = c("Unvaccinated", "1 dose", "2 doses"))),
    mapping = aes(
      x = as.numeric(age),
      y = value,
      fill = vaccination
    )
  ) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(
      x = "Age",
      y = "Population",
      fill = "",
      title = "Measles vaccination coverage (2024)"
    ) +
    scale_y_continuous(labels = scales::comma)
  
  protection_by_age <- ggplot(
    data = susceptibility_data %>%
      subset(time == max(time)),
    mapping = aes(
      x = as.numeric(age),
      y = value,
      fill = status
    )
  ) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(
      x = "Age",
      y = "Population",
      fill = "",
      title = "Measles susceptibility (2024)"
    )
  
}