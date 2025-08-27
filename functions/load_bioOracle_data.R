
load_bioOracle_layers <- function(scen, yr, path, db) {
  # Set layer names
  bot_names <- c(bot_temp = "thetao_mean", bot_salin = "so_mean", bot_ph = "ph_mean", bot_do = "o2_mean")
  surf_names <- c(surf_temp = "thetao_mean", surf_salin = "so_mean", surf_ph = "ph_mean", surf_do = "o2_mean")
  
  # Read terrain data
  terrain <- biooracle::read_terrain(
    what = c("aspect", "bathymetry_mean", "slope", "terrain_ruggedness_index", "topographic_position_index"),
    path = path,
    point = NA,
    crs = 4326
  )
  
  # Filter and load bottom layers
  db_bot <- dplyr::filter(db, .data$scenario == scen, .data$year == yr, .data$z == "depthmean", .data$trt == "mean")
  bot_layers <- read_biooracle(db_bot, path) |> dplyr::rename(dplyr::all_of(bot_names))
  
  # Filter and load surface layers
  db_surf <- dplyr::filter(db, .data$scenario == scen, .data$year == yr, .data$z == "depthsurf", .data$trt == "mean")
  surf_layers <- read_biooracle(db_surf, path) |> dplyr::rename(dplyr::all_of(surf_names))
  
  # Combine
  combined_layers <- c(surf_layers, bot_layers, terrain, along = NA_integer_)
  return(combined_layers)
}
