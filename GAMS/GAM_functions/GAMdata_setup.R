get_GAM_data = function(data_path){
  data <- readxl::read_excel(data_path)
  data$ScaledAbundance <- round(data$ScaledAbundance)
  data <- data[data$ScaledAbundance != 0, ]
  return(data)
}

get_GAM_spp_data = function(species, data){
  species_data <- data[data$Common_Name == species, ]
  
  # Return the filtered data
  return(species_data)
}

