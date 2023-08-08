library(leaflet)
library(sf)


# Load Yerevan city's shapefile data (replace the file path with your own)
yerevan_shapefile <- st_read('Grid_Yerevan', quiet = TRUE)
st_crs(yerevan_shapefile)
set.seed(123)

calculate_endpoints <- function(grid_polygon) {
  bbox <- st_bbox(grid_polygon)
  
  north_east <- c(bbox["xmax"], bbox["ymax"])
  south_east <- c(bbox["xmax"], bbox["ymin"])
  south_west <- c(bbox["xmin"], bbox["ymin"])
  north_west <- c(bbox["xmin"], bbox["ymax"])
  
  endpoints <- data.frame(NorthEast = north_east, SouthEast = south_east,
                          SouthWest = south_west, NorthWest = north_west)
  return(endpoints)
}

# Calculate the endpoints of each cell in the grid
endpoints_df <- lapply(st_geometry(grid_sf), calculate_endpoints)

# Combine all the endpoint dataframes into a single dataframe
endpoints_df <- do.call("rbind", endpoints_df)

# Print the dataframe with endpoint coordinates
print(endpoints_df)


# Function to generate random points within a specified bounding box
generate_random_points_within_boundary <- function(num_points, shapefile) {
  # Extract minimum and maximum latitude and longitude from the shapefile data
  min_lon <- south_west["xmin"]
  min_lat <- south_east["ymin"]
  max_lon <- north_east["xmax"]
  max_lat <- north_west["ymax"]
  
  # Generate random longitude and latitude coordinates
  random_lon <- runif(num_points, min_lon, max_lon)
  random_lat <- runif(num_points, min_lat, max_lat)
  
  # Combine longitude and latitude into a matrix
  random_points <- cbind(random_lon, random_lat)
  
  return(random_points)
}

# Example usage
# Generate 100 random points within the Yerevan city boundary
num_points <- 23
random_points <- generate_random_points_within_boundary(num_points, yerevan_shapefile)

# Convert the random points matrix to a spatial data frame
random_points_df <- data.frame(lon = random_points[, 1], lat = random_points[, 2])
coordinates(random_points_df) <- c("lon", "lat")
random_points_sf <- st_as_sf(random_points_df, coords = c('lon', 'lat'), crs = st_crs(yerevan_shapefile))
#st_crs(random_points_df) <- st_crs(yerevan_shapefile)  # Set the same CRS as the shapefile

# Create the leaflet map
map <- leaflet(yerevan_shapefile) %>%
  addPolygons() %>%
  addCircleMarkers(data = random_points_sf,  # Add the random points to the map
                   radius = 5,              # Marker size
                   color = "red",           # Marker color
                   fillOpacity = 0.8)       # Marker fill opacity

# Display the map
map