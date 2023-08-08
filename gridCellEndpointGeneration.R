# Load required packages
library(sf)

# Read the shapefile containing the 35 polygons
grid_sf <- st_read("Grid_Yerevan")

# Function to calculate the endpoints' longitude and latitude for a polygon
calculate_endpoints <- function(polygon) {
  bbox <- st_bbox(polygon)
  
  north_east <- c(x = bbox["xmax"], y = bbox["ymax"])
  south_east <- c(x = bbox["xmax"], y = bbox["ymin"])
  south_west <- c(x = bbox["xmin"], y = bbox["ymin"])
  north_west <- c(x = bbox["xmin"], y = bbox["ymax"])
  
  endpoints <- data.frame(
    NorthEast_Lon = north_east["x"], NorthEast_Lat = north_east["y"],
    SouthEast_Lon = south_east["x"], SouthEast_Lat = south_east["y"],
    SouthWest_Lon = south_west["x"], SouthWest_Lat = south_west["y"],
    NorthWest_Lon = north_west["x"], NorthWest_Lat = north_west["y"]
  )
  
  return(endpoints)
}

# Initialize an empty dataframe to store the endpoints
endpoints_df <- data.frame()

# Iterate through each polygon and calculate the endpoints
for (i in 1:nrow(grid_sf)) {
  polygon <- grid_sf[i,]
  cell_endpoints <- calculate_endpoints(polygon)
  
  # Add the endpoints to the dataframe
  endpoints_df <- rbind(endpoints_df, cell_endpoints)
}

# Print the dataframe with endpoints' longitude and latitude for each polygon
print(endpoints_df)

# Save the dataframe as a CSV file (optional)
write.csv(endpoints_df, "endpoints_dataframe.csv", row.names = FALSE)
