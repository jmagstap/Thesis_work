get_baseline_means <- function(variables, folder_path) {
  baseline_data <- data.frame()
  
  for (var in variables) {
    file_name <- paste0("baseline_2010_depthmean_", var, "_mean.tif")
    file_path <- file.path(folder_path, file_name)
    
    if (file.exists(file_path)) {
      r <- terra::rast(file_path)
      mean_val <- terra::global(r, fun = "mean", na.rm = TRUE)[[1]]
      
      baseline_data <- rbind(baseline_data, data.frame(
        scenario = "Baseline",
        decade = 2010,
        variable = var,
        mean_value = mean_val
      ))
    } else {
      warning(paste("Missing baseline file:", file_path))
    }
  }
  
  return(baseline_data)
}






get_decadal_means <- function(scenario, variables, decades, folder_path) {
  data <- data.frame()
  
  for (decade in decades) {
    for (var in variables) {
      file_name <- paste0("ssp", scenario, "_", decade, "_depthmean_", var, "_mean.tif")
      file_path <- file.path(folder_path, file_name)
      
      if (file.exists(file_path)) {
        r <- terra::rast(file_path)
        mean_val <- terra::global(r, fun = "mean", na.rm = TRUE)[[1]]
        
        data <- rbind(data, data.frame(
          scenario = paste0("SSP", scenario),
          decade = decade,
          variable = var,
          mean_value = mean_val
        ))
      } else {
        warning(paste("Missing file:", file_path))
      }
    }
  }
  
  return(data)
}
