library(sf)

# Load the Yerevan boundary shapefile
yerevan <- st_read("Yerevan Districts")

# Define the number of circles
num_circles <- 10

# Generate 10 random points within the Yerevan boundary
set.seed(123) # Setting a seed for reproducibility
##random_points <- st_sample(yerevan, size = num_circles, type = "random")

random_points <- data.frame(lon = c(44.511226,44.402389,44.494400,44.599800,
                                    44.579417,44.458351),
                            lat = c(40.165722,40.149687,40.092718,40.133153,
                                    40.220342,40.224437))


random_points <- st_as_sf(random_points, coords = c("lon", "lat"), crs = st_crs(yerevan))



# Define the buffer distance (1 km in meters)
radius_km <- 6
radius_meters <- radius_km * 1000

# Create circles around each random point as a spatial object
grid_circles <- st_buffer(random_points, dist = radius_meters)

# Convert the circles to a simple feature data frame (SFDF)
grid_circles_sfdf <- st_sf(geometry = grid_circles)

# Write the grid of circles to a shapefile
st_write(grid_circles_sfdf, "grid_circles_1km_yerevan.shp")
