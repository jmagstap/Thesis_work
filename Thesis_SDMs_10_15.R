library(lwgeom)
library(tidyverse)
library(yaml)
library(raster)
library(robis)
library(stars)
library(dismo)
library(terra)
library(maxnet)
library(ggplot2)
library(leaflet)
library(leafem)
library(mgcv)

# Load YAML configuration
vars <- read_yaml("/Users/lizamaguire/Desktop/R_data/R files/config2 (1).yaml")

# Function to get species data and convert to sf object
get_species_data <- function(spec) { 
  species_data <- read.csv("/Users/lizamaguire/Desktop/R_data/CSV files/Updated MeDMR data 10_10.csv")
  species_data$Date <- ymd_hms(species_data$Date)
  
  # Extract the month and create a new column
  species_data$month <- month(species_data$Date)
  
  # Filter based on years in YAML config
  filtered_data <- subset(species_data, Year >= vars$start_year & Year <= vars$end_year)
  
  # Create a presence/absence column
  filtered_data <- filtered_data %>%
    mutate(
      Number_Caught = as.numeric(Number_Caught),
      pa = ifelse(Common_Name == spec & Number_Caught > 0, 1, 0)
    )
  
  # Select relevant columns
  subset_data <- dplyr::select(filtered_data, Latitude, Longitude, Year, Season, Number_Caught, Common_Name, pa)
  
  # Convert to sf object
  obs_sf <- subset_data %>% 
    sf::st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = st_crs(4326))
  return(obs_sf)
}

# Function to get environmental data and crop it
get_enviro_data <- function(envvars) {
  # Load environmental layers
  env <- sdmpredictors::load_layers(envvars, equalarea = FALSE, rasterstack = TRUE)
  
  # Convert environmental data to stars object
  env <- st_as_stars(env)
  
  # Define bounding box based on lon/lat min and max from vars
  extent <- st_bbox(c(xmin = vars$lonmin, xmax = vars$lonmax, ymin = vars$latmin, ymax = vars$latmax), crs = st_crs(env))
  
  # Warp the environmental raster to match the extent's CRS and bounding box
  env_warped <- st_warp(env, dest = st_as_stars(extent))
  
  # Crop the warped environmental raster
  rc <- st_crop(x = env_warped, y = extent)
  
  return(rc)
}

# Function to extract environmental data at species point locations
extractEnvData <- function(rasterStack, points) {
  env.stars <- terra::split(rasterStack)
  spec.env <- stars::st_extract(env.stars, sf::st_coordinates(points))
  spec.env <- na.omit(spec.env)  # Remove NAs from extraction
  return(spec.env)
}

# Function to get negative points from species dataset where pa = 0
getNegativePoints <- function(species_data_sf) {
  # Filter the dataset to get only points where pa = 0
  negative_points <- speciesPoints %>% filter(pa == 0)
  # Check if there are any negative points
  if (nrow(negative_points) == 0) {
    stop("No negative points (pa = 0) found in the dataset.")
  }
  
  # Return the negative points as an sf object
  return(negative_points)
}


# Load environmental variables from vars
layers <- c(vars$envVars)

# Get environmental raster stack
envRasterStack <- get_enviro_data(layers)

# Get species observation points for "Haddock"
speciesPoints <- get_species_data("Haddock")

# Get random background (pseudoabsence) points
absPoints <- getNegativePoints(speciesPoints)

# Extract environmental data at species points and assign pa = 1
pres <- extractEnvData(envRasterStack, speciesPoints) |> mutate(pa = 1)

# Extract environmental data at background points and assign pa = 0
abs <- extractEnvData(envRasterStack, absPoints) |> mutate(pa = 0)

# Combine presence and pseudoabsence data
allData <- rbind(pres, abs)

# View combined data
head(allData)

presence_absence_df <- allData %>%
  dplyr::select(pa)

environmental_df <- allData %>%
  dplyr::select(-c(pa))

# Ensure that 'Presence' column is extracted as a numeric vector
presence_absence_vector <- presence_absence_df$pa

model_data <- cbind(pa = presence_absence_vector, environmental_df)
library(mgcv)

# Fit the species distribution model using GAM
sdm_model <- gam(
  pa ~ s(BO21_tempmean_bdmax) + s(BO22_salinitymean_bdmax),
  data = model_data,
  family = binomial(link = "logit")
)

# Summarize the model
summary(sdm_model)

# Convert the environmental raster stack from stars object to data.frame
environmental_df <- as.data.frame(envRasterStack, na.rm = TRUE, xy = TRUE)


# Predict habitat suitability using the fitted model
# Create a prediction grid based on the raster stack
pred_grid <- environmental_df

# Check column names in the prediction grid
names(pred_grid)

# Rename columns if necessary to match the GAM model variables
colnames(pred_grid)[which(names(pred_grid) == "BO21_tempmean")] <- "BO21_tempmean_bdmax"
colnames(pred_grid)[which(names(pred_grid) == "BO22_salinitymean")] <- "BO22_salinitymean_bdmax"


# Predict the suitability (0-1 scale) over the raster stack
pred_suitability <- predict(sdm_model, newdata = pred_grid, type = "response")

# Add predictions back to the raster stack
pred_raster <- rast(pred_suitability, template = envRasterStack)

