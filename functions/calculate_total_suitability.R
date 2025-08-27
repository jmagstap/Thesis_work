save_predictions_and_calculate_suitability <- function(model,
                                                       scenario_layers_list,
                                                       scenario_names,
                                                       decade_names,
                                                       species_name = "lobster",
                                                       save_dir = "~/Desktop/Thesis_data/Final SDM models/GAM",
                                                       threshold = 0.7,
                                                       plot_maps = TRUE) {
  
  # Create species directory if it doesn't exist
  species_dir <- file.path(save_dir, species_name)
  if (!dir.exists(species_dir)) {
    dir.create(species_dir, recursive = TRUE)
  }
  
  results <- data.frame(
    scenario = character(),
    decade = numeric(),
    total_suitability = numeric(),
    stringsAsFactors = FALSE
  )
  
  nbreaks <- 100  # for plotting
  
  for (scenario in scenario_names) {
    for (decade in decade_names) {
      
      layers <- scenario_layers_list[[scenario]][[as.character(decade)]]
      
      # Predict
      prediction <- predict_raster(model, newdata = layers, type = "response")
      
      # Save raster to file
      filename <- paste0(species_name, "_gam_", decade, "_", gsub("-", "_", scenario), ".tif")
      filepath <- file.path(species_dir, filename)
      write_stars(prediction, filepath)
      
      # Optional plot
      if (plot_maps) {
        plot(prediction,
             main = paste0(tools::toTitleCase(species_name), " ", decade, " ", scenario),
             nbreaks = nbreaks,
             col = hcl.colors(nbreaks - 1, palette = "viridis"),
             key.length = 1,
             axes = TRUE)
      }
      
      # --- Total map-wide suitability ---
      values <- prediction[[1]]
      total_cells <- sum(!is.na(values), na.rm = TRUE)
      n_suitable <- sum(values >= threshold, na.rm = TRUE)
      percent_suitable <- 100 * n_suitable / total_cells
      
      results <- rbind(results, data.frame(
        scenario = scenario,
        decade = as.numeric(decade),
        total_suitability = total
      ))
    }
  }
  
  return(results)
}




updated_calculate_suitability <- function(model,
                                          scenario_layers_list,
                                          scenario_names,
                                          decade_names,
                                          species_name = "lobster",
                                          save_dir = "~/Desktop/Thesis_data/Final SDM models/ecoregions_GAM",
                                          threshold = 0.7,
                                          plot_maps = TRUE,
                                          region_shapes = NULL) {
  library(stars)
  library(sf)
  library(dplyr)
  library(ggplot2)
  
  # Create species directory if it doesn't exist
  species_dir <- file.path(save_dir, species_name)
  if (!dir.exists(species_dir)) dir.create(species_dir, recursive = TRUE)
  
  results <- data.frame()
  nbreaks <- 100
  
  if (!is.null(region_shapes)) {
    region_shapes <- st_make_valid(region_shapes)
  }
  
  for (scenario in scenario_names) {
    available_decades <- names(scenario_layers_list[[scenario]])
    for (decade in decade_names) {
      if (!(as.character(decade) %in% available_decades)) next
      
      layers <- scenario_layers_list[[scenario]][[as.character(decade)]]
      prediction <- predict_raster(model, newdata = layers, type = "response")
      
      # Align CRS if needed
      if (!is.null(region_shapes) && !st_crs(prediction) == st_crs(region_shapes)) {
        region_shapes <- st_transform(region_shapes, st_crs(prediction))
      }
      
      # Save prediction raster
      filename <- paste0(species_name, "_gam_", decade, "_", gsub("-", "_", scenario), ".tif")
      filepath <- file.path(species_dir, filename)
      write_stars(prediction, filepath)
      
      # Extract values
      values <- prediction[[1]]
      total_cells <- sum(!is.na(values), na.rm = TRUE)
      n_suitable <- sum(values >= threshold, na.rm = TRUE)
      percent_suitable <- 100 * n_suitable / total_cells
      
      # Store total suitability for the scenario-decade
      total_decade_suitable <- n_suitable
      
      # Add "Total" row
      results <- bind_rows(results, tibble(
        scenario = scenario,
        decade = as.numeric(decade),
        region = "Total",
        n_suitable = n_suitable,
        total_cells = total_cells,
        percent_suitable = percent_suitable,
        total_decade_suitable = total_decade_suitable
      ))
      
      # Plot map
      if (plot_maps) {
        pred_df <- as.data.frame(prediction, xy = TRUE)
        names(pred_df)[3] <- "suitability"
        
        p <- ggplot() +
          geom_raster(data = pred_df, aes(x = x, y = y, fill = suitability)) +
          scale_fill_viridis_c(option = "viridis", name = "Suitability", na.value = "transparent") +
          labs(
            title = paste(tools::toTitleCase(species_name), decade, scenario),
            x = "Longitude", y = "Latitude"
          ) +
          coord_sf() +
          theme_minimal()
        
        if (!is.null(region_shapes)) {
          p <- p + geom_sf(data = region_shapes, fill = NA, color = "red", size = 2)
        }
        
        print(p)
      }
      
      # Region-wise suitability summary
      if (!is.null(region_shapes)) {
        for (i in seq_len(nrow(region_shapes))) {
          region <- region_shapes[i, ]
          region_name <- as.character(region$region)
          
          # Clip prediction to region
          region_crop <- suppressWarnings(st_crop(prediction, region))
          region_masked <- region_crop
          region_masked[[1]][!st_intersects(st_as_sf(region_crop, as_points = TRUE), region, sparse = FALSE)] <- NA
          
          if (!inherits(region_masked, "stars")) next
          
          region_values <- region_masked[[1]]
          region_total <- sum(!is.na(region_values), na.rm = TRUE)
          region_suitable <- sum(region_values >= threshold, na.rm = TRUE)
          region_percent <- if (region_total > 0) 100 * region_suitable / region_total else NA_real_
          
          results <- bind_rows(results, tibble(
            scenario = scenario,
            decade = as.numeric(decade),
            region = region_name,
            n_suitable = region_suitable,
            total_cells = region_total,
            percent_suitable = region_percent,
            total_decade_suitable = total_decade_suitable
          ))
        }
      }
    }
  }
  
  # Assign results to global environment with name like "lobster_results"
  assign(paste0(species_name, "_results"), results, envir = .GlobalEnv)
  
  return(results)
}
