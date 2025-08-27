bot_do_y2k = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2000/baseline_2000_depthmean_o2_mean.tif")
bot_do_y2k = st_set_crs(bot_do_y2k, 4326)

surf_do_y2k = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2000/baseline_2000_depthsurf_o2_mean.tif")
surf_do_y2k = st_set_crs(surf_do_y2k, 4326)

bot_ph_y2k = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2000/baseline_2000_depthmean_ph_mean.tif")
bot_ph_y2k = st_set_crs(bot_ph_y2k, 4326)

surf_ph_y2k = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2000/baseline_2000_depthsurf_ph_mean.tif")
surf_ph_y2k = st_set_crs(surf_ph_y2k, 4326)



bot_do_2010 = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2010/baseline_2010_depthmean_o2_mean.tif")
bot_do_2010 = st_set_crs(bot_do_2010, 4326)

surf_do_2010 = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2010/baseline_2010_depthsurf_o2_mean.tif")
surf_do_2010 = st_set_crs(surf_do_2010, 4326)

bot_ph_2010 = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2010/baseline_2010_depthmean_ph_mean.tif")
bot_ph_2010 = st_set_crs(bot_ph_2010, 4326)

surf_ph_2010 = read_stars("~/Desktop/Thesis_data/Biooraclev3/NES/baseline/2010/baseline_2010_depthsurf_ph_mean.tif")
surf_ph_2010 = st_set_crs(surf_ph_2010, 4326)


terrain = biooracle::read_terrain(
  what = c("aspect", "bathymetry_mean", "slope", "terrain_ruggedness_index",
           "topographic_position_index"),
  path = path,
  point = NA,
  crs = 4326
)


extract_env_vars <- function(species) {
  # Read and prepare base dataset
  sdm_data <- read_DMR() |> bb_DMR()
  species_data <- pa_species3(s = sdm_data, species = species)
  species_data <- st_as_sf(species_data) |> st_set_crs(4326)
  
  # Extract terrain variables
  values <- st_extract(terrain, species_data) |> 
    st_drop_geometry() |>  
    as_tibble()
  species_data <- bind_cols(species_data, values) |> relocate(geometry, .after = last_col())
  
  # Add decade column
  species_data <- species_data |> mutate(decade = floor(year / 10) * 10)
  
  # Add columns for additional variables
  species_data$surf_do <- NA
  species_data$bot_do <- NA
  species_data$surf_ph <- NA
  species_data$bot_ph <- NA
  
  # Set indexes for decades
  index_2000 <- species_data$decade == 2000
  index_2010 <- species_data$decade == 2010
  
  # Extract environmental variables for 2000s
  species_data$surf_do[index_2000] <- as.data.frame(st_extract(surf_do_y2k, species_data[index_2000, ]))[[1]]
  species_data$bot_do[index_2000] <- as.data.frame(st_extract(bot_do_y2k, species_data[index_2000, ]))[[1]]
  species_data$surf_ph[index_2000] <- as.data.frame(st_extract(surf_ph_y2k, species_data[index_2000, ]))[[1]]
  species_data$bot_ph[index_2000] <- as.data.frame(st_extract(bot_ph_y2k, species_data[index_2000, ]))[[1]]
  
  # Extract environmental variables for 2010s
  species_data$surf_do[index_2010] <- as.data.frame(st_extract(surf_do_2010, species_data[index_2010, ]))[[1]]
  species_data$bot_do[index_2010] <- as.data.frame(st_extract(bot_do_2010, species_data[index_2010, ]))[[1]]
  species_data$surf_ph[index_2010] <- as.data.frame(st_extract(surf_ph_2010, species_data[index_2010, ]))[[1]]
  species_data$bot_ph[index_2010] <- as.data.frame(st_extract(bot_ph_2010, species_data[index_2010, ]))[[1]]
  
  # Write CSV to desktop folder
  output_path <- paste0("~/Desktop/Thesis_data/4.28.25.species.data/", tolower(species), "_sdm_data.gpkg")
  st_write(species_data, output_path)
}
