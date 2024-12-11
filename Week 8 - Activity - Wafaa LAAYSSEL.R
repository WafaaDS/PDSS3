install.packages("tidyverse")
install.packages("purrr")
install.packages("scatterplot3d")  
library(scatterplot3d) 
library(tidyverse)
library(ggplot2)
library(maps)

clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")

# structure of the dataset
str(clim)
summary(clim)

# Overview of the columns altitude and p_mean
summary(clim$altitude)
summary(clim$p_mean)

# they are character column and needs to be transformed to numeric
clim$altitude <- as.numeric(clim$altitude)
clim$p_mean <- as.numeric(clim$p_mean)

# 


# Get map data for France
france_map <- map_data("france")

# Plot the stations
ggplot() +
  geom_polygon(data = france_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  geom_point(data = clim, aes(x = lon, y = lat, color = t_mean), 
             size = 3) +
  scale_color_gradient(low = "blue", high = "red", name = "Mean Temp") +
  labs(title = "Climate Stations in France", x = "Longitude", y = "Latitude") +
  theme_minimal()

# EXERCICE 1

# Excluding if the two high mountains Mont-Ventoux and Pic-du-Midi
climfrar <- clim[1:34, ]

# Let's run the linear model
MAT <- lm(t_mean ~ altitude + lat + lon, data = climfrar)

# Summary of the model
summary(MAT)

# EXERCICE 2

MAT2 <- lm(t_mean ~ altitude + lat, data = clim)
summary(MAT2)

# Since the traget stations are missing values of altitude we will assign them manually

clim$altitude[clim$station == "Mont-Ventoux"] <- 1912
clim$altitude[clim$station == "Pic-du-Midi"] <- 2877

# Now we try to predict
target_stations <- clim[clim$station %in% c("Mont-Ventoux", "Pic-du-Midi"), ]
print(target_stations)

predictions <- predict(MAT2, target_stations, interval = "confidence")

# predictions and  actual data
comparison <- cbind(
  target_stations[, c("station", "altitude", "lat", "t_mean")],
  Predicted = predictions[, "fit"],
  Lower_CI = predictions[, "lwr"],
  Upper_CI = predictions[, "upr"]
)

print(comparison)

# EXERCICE 3

# 3D Scatterplot

scatterplot3d(
  x = clim$altitude, y = clim$lat, z = clim$t_mean,
  xlab = "Altitude (m)", ylab = "Latitude (°)", zlab = "Mean Temperature (°C)",
  main = "3D Scatterplot of Spatial Attributes and Temperature",
  color = "blue", pch = 19  
)
summary(MAT2)




