create_sdm_animation <- function(species_name, species_path, 
                                 scenarios_to_include = c("baseline", "SSP5-8.5", "SSP2-4.5"),
                                 output_path = ".") {
  
  # Step 1: List TIF files
  tif_files <- list.files(
    path = species_path,
    pattern = "\\.tif$",
    full.names = TRUE
  )
  
  # Step 2: Extract metadata from filenames
  file_info <- tibble(
    file = tif_files,
    filename = basename(tif_files)
  ) %>%
    mutate(
      species = species_name,
      model = str_extract(filename, "(?<=_)[^_]+(?=_)"),
      year = str_extract(filename, "_\\d{4}_") %>% str_extract("\\d{4}") %>% as.integer(),
      scenario = case_when(
        str_detect(filename, "SSP5[-_\\.]?(8\\.5|85)") ~ "SSP5-8.5",
        str_detect(filename, "SSP2[-_\\.]?(4\\.5|45)") ~ "SSP2-4.5",
        str_detect(filename, "baseline") ~ "baseline",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(scenario %in% scenarios_to_include) %>%
    # ✅ Drop filename to prevent pmap error
    dplyr::select(file, species, model, year, scenario)
  
  # Step 3: Raster to data frame conversion
  raster_to_df <- function(file, species, model, year, scenario) {
    r <- terra::rast(file)
    df <- as.data.frame(r, xy = TRUE)
    names(df)[3] <- "suitability"
    df$species <- species
    df$model <- model
    df$year <- year
    df$scenario <- scenario
    return(df)
  }
  
  # Step 4: Combine all rasters
  all_rasters_df <- purrr::pmap_dfr(file_info, raster_to_df)
  
  # Step 5: Arrange for plotting
  plot_df <- all_rasters_df %>%
    arrange(scenario, year)
  
  # Step 6: Build animation
  anim_plot <- ggplot(plot_df, aes(x = x, y = y, fill = suitability)) +
    geom_raster() +
    scale_fill_viridis_c(option = "viridis", name = "Suitability", limits = c(0, 1)) +
    coord_fixed() +
    theme_minimal() +
    labs(
      title = paste0(species_name, " GAM Forecast — Year: {frame_time} | Scenario: {closest_state}"),
      x = "Longitude", y = "Latitude"
    ) +
    transition_states(scenario, transition_length = 2, state_length = 1) +
    transition_time(year) +
    ease_aes("linear")
  
  # Step 7: Save animation
  filename <- paste0(gsub(" ", "_", tolower(species_name)), "_animation.gif")
  full_path <- file.path(output_path, filename)
  
  animate(anim_plot, fps = 15, width = 800, height = 600,
          renderer = gifski_renderer(full_path))
  
  message("Saved animation to: ", full_path)
}
