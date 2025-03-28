
demographic_data_processing <- function(
    file_name,
    country_subset = "",
    year_subset_start = "",
    year_subset_end = "",
    save = F,
    save_location = ""
){
  
  #Import file
  file <- import(file_name)
  
  #Find start of the data
  first_non_NA <- first(which(!is.na(file[, 1])))
  
  #Subset and rename with the correct columns
  file_clean <- file[(first_non_NA + 1):nrow(file), ] %>%
    set_names(file[first_non_NA, ]) %>%
    clean_names()
  
  #Set year subset
  years <- if(year_subset_start == "" & year_subset_end == ""){
    as.numeric(min(file_clean$year, na.rm = T)):as.numeric(max(file_clean$year, na.rm = T))
  } else if(year_subset_start != "" & year_subset_end == ""){
    as.numeric(year_subset_start):as.numeric(max(file_clean$year, na.rm = T))
  } else if(year_subset_start == "" & year_subset_end != ""){
    as.numeric(min(file_clean$year, na.rm = T)):as.numeric(year_subset_end)
  } else if(year_subset_start != "" & year_subset_end != ""){
    as.numeric(year_subset_start):as.numeric(year_subset_end)
  }
  
  #Set up country subset
  countries <- if(country_subset == ""){
    unique(file_clean$region_subregion_country_or_area)
  } else {
    country_subset
  }
  
  #Overall subset
  overall_subset <- file_clean %>%
    filter(region_subregion_country_or_area %in% countries & year %in% years)

  #Save 
  if(save == T){
    
    folder_location <- if(save_location == "") dirname(file_name) else save_location
    
    filename <- tolower(gsub(" ", "", paste(country_subset, year_subset_start, year_subset_end, sep = "_")))
    
    full_filename <- paste0(folder_location, "/", filename, tolower(gsub(".xlsx", "", last(unlist(strsplit(file_name, "/")[[1]])))), ".csv")
    
    export(overall_subset, full_filename)
    
  }

}