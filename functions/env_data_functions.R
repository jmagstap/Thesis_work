#Input: variables, output: raster stack
get_enviro_data <- function(envvars) {
  #layercodes <- var
  #dir = "ohw24_proj_sdm_us"
  env <- sdmpredictors::load_layers(envvars, equalarea = FALSE, rasterstack = TRUE)
  #Crop
  env <- st_as_stars(env)
  extent <- st_bbox(c(xmin = vars$lonmin, xmax = vars$lonmax, ymin = vars$latmin, ymax = vars$latmax), crs = st_crs(env))
  env_warped <- st_warp(env, dest = st_as_stars(extent))
  rc <- st_crop(x = env_warped, y = extent)
  return(rc)
}

#extract specific environmental covariates 
#ph <- get_enviro_data("BO_ph")
extractEnvData <- function(rasterStack, points) {
  env.stars <- terra::split(rasterStack)
  spec.env <- stars::st_extract(env.stars, sf::st_coordinates(points))
  spec.env.cleaned <- na.omit(spec.env)
  return(spec.env.cleaned)
}