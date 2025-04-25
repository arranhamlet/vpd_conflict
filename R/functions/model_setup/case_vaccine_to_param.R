case_vaccine_to_param <- function(
    demog_data,
    processed_vaccination,
    processed_case
){
  
  #Set up parameters
  n_age <- demog_data$input_data$n_age
  n_vacc <- demog_data$input_data$n_vacc
  n_risk <- demog_data$input_data$n_risk
  
  years <- demog_data$input_data$year_start:demog_data$input_data$year_end
  
  #Process vaccination first
  sapply(1:nrow(processed_vaccination), function(x){
    
    
  }, simplify = FALSE)
  
  vaccination_df <- data.frame(
    dim1 = ,
    dim2 = ,
    dim3 = ,
    dim4 = ,
    value = ,
  )
  
}