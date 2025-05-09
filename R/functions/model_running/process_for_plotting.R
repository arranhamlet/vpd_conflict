
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

  list(aggregate_df = aggregate_df,
       susceptibility_data = susceptibility_data)
  
}