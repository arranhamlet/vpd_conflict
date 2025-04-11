
process_vaccination <- function(
    vaccination_data,
    iso,
    vaccine = "All",
    year_start = "",
    year_end = ""
  ){
  
  # Ensure data.table format
  setDT(vaccination_data)
  
  # Subset to country and year range
  years <- get_years(vaccination_data$year, year_start, year_end)
  
  filtered <- vaccination_data[iso3 == iso & year %in% years]
  
  # Subset vaccine
  if (vaccine != "All") {
    filtered <- filtered[name == vaccine]
  }
  
  return(filtered)
  
}