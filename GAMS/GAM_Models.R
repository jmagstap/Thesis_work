#Generalized additive model to relate species presence to bottom temperature
getwd()
setwd("/Users/lizamaguire/Desktop/R_data")

# Load the necessary libraries
library(mgcv)
library(mgcViz)
library(dplyr)
library(ggplot2)
library(gratia)

# Read the data from CSV file (modify the path to your data)
temp_data <- read.csv("/Users/lizamaguire/Desktop/R_data/No zero data/temp_no_zero.csv")

# Convert 'Number_Caught' to numeric
temp_data$Number_Caught <- as.numeric(temp_data$Number_Caught)

# Create a column 'butterfish_presence' to show binomial presence/absence data for butterfish
temp_data <- temp_data %>%
  mutate(redhake_presence = ifelse(Common_Name == "Hake Atlantic Red" & Number_Caught > 0, 1, 0))

# Fit the GAM model
gam_model <- gam(redhake_presence ~ s(Bottom.Temperature, k = 20), family = binomial(), data = temp_data)

# Summarize the model
summary(gam_model)

temp_viz_model <- getViz(gam_model)
# Plot the smooth term with a dotted line at zero and shaded confidence interval
plot(sm(temp_viz_model, 1)) + 
  l_fitLine() +                      # Add the fitted line
  l_ciPoly(fill = "blue", alpha = 0.2) +  # Add the shaded confidence interval
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +  # Add a dotted line at zero
  l_rug() +  
  theme_minimal() +
labs(x = "Bottom temperature (C)", y = "Probability of red hake suitability")  # Add axis titles and plot title
#check the appropriateness of the k smooth term 
gam.check(gam_model)



#Generalized additive model to relate species presence to bottom salinity
getwd()
setwd("/Users/lizamaguire/Desktop/R_data")
#load necessary packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(ggeffects)
sal_data <- read.csv("/Users/lizamaguire/Desktop/R_data/No zero data/salinity_no_zero.csv")

#turn number_caught column into numeric
sal_data$Number_Caught <- as.numeric(sal_data$Number_Caught)

#create column Alewife_presence to show binomial presence/absence data for alewife
sal_data <- sal_data %>%
  mutate(redhake_presence = ifelse(Common_Name == "Hake Atlantic Red" & Number_Caught > 0, 1, 0))

# Check the range of Bottom.Salinity
range(sal_data$Bottom.Salinity, na.rm = TRUE)

# Fit the GAM model with increased basis dimension
sal_gam_model <- gam(redhake_presence ~ s(Bottom.Salinity, k = 10), family = binomial(), data = sal_data)

# Summarize statistics for the model
summary(sal_gam_model)

# Visualize the model
sal_viz_model <- getViz(sal_gam_model)

# Plot the smooth term with a dotted line at zero and shaded confidence interval
plot(sm(sal_viz_model, 1)) + 
  l_fitLine() +                      # Add the fitted line
  l_ciPoly(fill = "blue", alpha = 0.2) +  # Add the shaded confidence interval
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +  # Add a dotted line at zero
  l_rug() +                          # Add the rug plot
  theme_minimal() +
  labs(x = "Bottom salinity (ppt)", y = "Probability of red hake suitability")  # Add axis titles
gam.check(sal_gam_model)

linear_model <- glm(redhake_presence ~ Bottom.Salinity, data = sal_data, family = binomial())
# Compare the models
AIC(sal_viz_model, linear_model)
#gam model is better fit for this data


#Generalized additive model to relate species presence to depth
getwd()
setwd("/Users/lizamaguire/Desktop/R_data")
install.packages("mgcv")
install.packages("ggplot2")
#load necessary packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(mgcViz)

# Read the data
depth_data <- read.csv("/Users/lizamaguire/Desktop/R_data/No zero data/depth_no_zero.csv")

# Turn number_caught column into numeric and create pollock_presence column
depth_data <- depth_data %>%
  mutate(
    Number_Caught = as.numeric(Number_Caught),
    butterfish_presence = ifelse(Common_Name == "Butterfish" & Number_Caught > 0, 1, 0)
  )

# Fit the GAM model
depth_gam_model <- gam(butterfish_presence ~ s(Depth, k = 20), data = depth_data, family = binomial())

# Summarize statistics for model
summary(depth_gam_model)

# Visualize the model
depth_viz_model <- getViz(depth_gam_model)

# Plot the smooth term with a dotted line at zero and shaded confidence interval
plot(sm(depth_viz_model, 1)) + 
  l_fitLine() +  # Add the fitted line
  l_ciPoly(fill = "blue", alpha = 0.2) +  # Add the shaded confidence interval
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +  # Add a dotted line at zero
  l_rug() +  # Add the rug plot
  theme_minimal() +
  labs(x = "Depth", y = "Probability of butterfish presence")  # Add axis titles
gam.check(depth_gam_model)



#Plot the results with customized axis titles
plot(gam_model, se = TRUE, shade = TRUE,
     xlab = "Depth",
     ylab = "Probability of haddock Presence")

gratia::draw(gam_model)






#GAM including 2 predictor variables (bottom temp and salinity)
Data <- read.csv("/Users/lizamaguire/Desktop/R_data/CSV files/Updated_Catch_data_8_1_2024.csv")

#turn number_caught column into numeric
Data$Number_Caught <- as.numeric(Data$Number_Caught)

#create column Alewife_presence to show binomial presence/absence data for alewife
Data <- Data %>%
  mutate(Alewife_presence = ifelse(Common_Name == "Alewife" & Number_Caught > 0, 1, 0))

# Filter data for species presence (assuming species presence is indicated by 1)
presence_data <- Data[Data$Alewife_presence == 1, ]

gam_model <- gam(Alewife_presence ~ s(Bottom.Temperature) + s(Bottom.Salinity), data = presence_data)

#summarize statistics for model
summary(gam_model)

# Create a grid of values for Bottom.Temperature and Bottom.Salinity
temperature_grid <- seq(min(Data$Bottom.Temperature), max(Data$Bottom.Temperature), length.out = 100)
salinity_grid <- seq(min(Data$Bottom.Salinity), max(Data$Bottom.Salinity), length.out = 100)

# Create a data frame for prediction
prediction_grid <- expand.grid(Bottom.Temperature = temperature_grid, Bottom.Salinity = salinity_grid)

# Predict species distribution on the grid
predictions <- predict(gam_model, newdata = prediction_grid, type = "link", se.fit = TRUE)

# Transform predictions to response scale (probabilities)
predictions_response <- plogis(predictions$fit)
se_response <- plogis(predictions$fit + 1.96 * predictions$se.fit) - plogis(predictions$fit - 1.96 * predictions$se.fit)

# Add predictions and confidence intervals to the prediction grid
prediction_grid$species_pred <- predictions_response
prediction_grid$ci_low <- plogis(predictions$fit - 1.96 * predictions$se.fit)
prediction_grid$ci_high <- plogis(predictions$fit + 1.96 * predictions$se.fit)

ggplot(presence_data, aes(x = Bottom.Temperature, y = Bottom.Salinity)) +
  geom_point(color = "red", size = 3) +  # Plot species presence data points
  geom_raster(data = prediction_grid, aes(fill = species_pred), alpha = 0.5) +  # Plot predicted species distribution
  geom_contour(data = prediction_grid, aes(z = ci_low), linetype = "dashed", color = "blue") +  # Lower bound of CI
  geom_contour(data = prediction_grid, aes(z = ci_high), linetype = "dashed", color = "blue") +  # Upper bound of CI
  labs(x = "Bottom Temperature", y = "Bottom Salinity", title = "Alewife Presence GAM") +
  scale_fill_viridis_c(option = "plasma", name = "Predicted Presence") +  # Color scale for predictions
  theme_minimal()
