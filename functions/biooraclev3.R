#' List Available biooracle layers
#'
#' This function lists available SDM predictors based on the directory specified in 
#' the `sdmpredictors_datadir` option. It checks if the directory exists and returns 
#' a list of available predictors.
#'@param path A character string specifying the directory containing SDM predictors. 
#' If not provided, the function uses the option set in `~/Desktop/Thesis_data/SDMpredictors/Baseline_2000_2019/2000`. 
#' @return A list of available SDM predictors. If the directory does not exist or 
#' is not set, an error message is returned.
#' 

list_and_plot_rasters <- function(sdmdatadir = "~/Desktop/Thesis_data/SDMpredictors/Baseline_2000_2019/2000") {
  
  # Expand the '~' symbol to the full path
  sdmdatadir <- path.expand(sdmdatadir)
  
  # Check if the directory exists
  if (!dir.exists(sdmdatadir)) {
    stop("The specified directory does not exist.")
  }
  
  # List all files with common raster extensions (.tif, .grd, etc.)
  raster_files <- list.files(sdmdatadir, pattern = "\\.(tif|grd)$", full.names = TRUE)
  
  # Check if there are raster files
  if (length(raster_files) == 0) {
    stop("No raster files found in the specified directory.")
  }
  
  # List the raster file names
  print(paste("Found", length(raster_files), "raster files:"))
  print(raster_files)
  
  # Read and plot each raster
  for (raster_file in raster_files) {
    # Read the raster file
    r <- raster(raster_file)
    
    # Plot the raster using the plot function from the raster package
    plot(r, main = paste("Raster Layer:", basename(raster_file)))
  }
}




stack_rasters <- function(sdmdatadir = "~/Desktop/Thesis_data/SDMpredictors/Baseline_2000_2019/2000") {
  
  # Expand the '~' symbol to the full path
  sdmdatadir <- path.expand(sdmdatadir)
  
  # Check if the directory exists
  if (!dir.exists(sdmdatadir)) {
    stop("The specified directory does not exist.")
  }
  
  # List all files with common raster extensions (.tif, .grd, etc.)
  raster_files <- list.files(sdmdatadir, pattern = "\\.(tif|grd)$", full.names = TRUE)
  
  # Check if there are raster files
  if (length(raster_files) == 0) {
    stop("No raster files found in the specified directory.")
  }
  
  # List the raster file names
  print(paste("Found", length(raster_files), "raster files:"))
  print(raster_files)
  
  # Read all raster files and stack them
  raster_layers <- lapply(raster_files, raster)
  
  # Stack all the rasters
  raster_stack <- stack(raster_layers)
  
  # Plot the raster stack
  plot(raster_stack, main = "Stacked Raster Layers")
  
  # Return the stacked raster object
  return(raster_stack)
}

