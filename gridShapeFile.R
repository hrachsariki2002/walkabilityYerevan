library(sf)
library(ggplot2)

# Load the cleaned data and read the grid shapefile
clean_data <- read.csv("clean_data.csv")
arm <- st_read('Grid_Yerevan')

# Set CRS for the grid
arm <- st_set_crs(arm, 4326)

# Create sf object for the points
points_sf <- st_as_sf(clean_data, coords = c("lon", "lat"), crs = 4326)

# Transform both datasets to have the same CRS
points_sf <- st_transform(points_sf, crs = st_crs(arm))
arm <- st_transform(arm, crs = st_crs(points_sf))

# Perform spatial intersection and assign the district numbers to the points
points_sf$district <- as.integer(st_intersects(points_sf, arm))

# Plot the districts map
districtsmap <- ggplot() + 
  geom_sf(data = arm) +
  geom_sf(data = points_sf, aes(color = factor(district)))
districtsmap

# Update the cleaned data with the district information
clean_data$cell <- points_sf$district

write.csv(clean_data, "cellinfo.csv")
