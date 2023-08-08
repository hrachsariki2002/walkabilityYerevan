# Load necessary libraries
library(sf)

# Read the shapefile
yerevan_shapefile <- st_read('Grid_Yerevan', quiet = T)
grid_sf <- st_make_grid(yerevan_shapefile, n = c(5, 7), what = "polygons")

# Function to generate points at a certain distance from the endpoints
generate_points_in_cell <- function(cell_geom, distance) {
  center_point <- st_centroid(cell_geom)
  
  endpoint1 <- st_coordinates(st_cast(cell_geom, "POINT"))[1, ]
  endpoint2 <- st_coordinates(st_cast(cell_geom, "POINT"))[nrow(st_cast(cell_geom, "POINT")), ]
  
  bearing1 <- atan2(as.numeric(center_point["Y"]) - endpoint1["Y"], as.numeric(center_point["X"]) - endpoint1["X"])
  bearing2 <- atan2(as.numeric(center_point["Y"]) - endpoint2["Y"], as.numeric(center_point["X"]) - endpoint2["X"])
  
  point1 <- st_sfc(st_point(c(as.numeric(center_point["X"]) + distance * cos(bearing1), 
                              as.numeric(center_point["Y"]) + distance * sin(bearing1))), crs = st_crs(cell_geom))
  
  point2 <- st_sfc(st_point(c(as.numeric(center_point["X"]) + distance * cos(bearing2), 
                              as.numeric(center_point["Y"]) + distance * sin(bearing2))), crs = st_crs(cell_geom))
  
  return(list(point1, point2, center_point))
}

# Function to generate 5 points in each cell
generate_points_in_grid <- function(grid_sf, distance) {
  point_list <- vector("list", length(grid_sf))
  for (i in 1:length(grid_sf)) {
    cell_geom <- st_geometry(grid_sf[i])
    points_in_cell <- generate_points_in_cell(cell_geom, distance)
    point_list[[i]] <- points_in_cell
  }
  return(point_list)
}

# Generate the points in each cell (500 meters distance from endpoints)
distance_from_endpoints <- 500  # Change this value as needed
points_list <- generate_points_in_grid(grid_sf, distance_from_endpoints)

# Convert the list of points to individual sf objects for each cell
points_sf_list <- lapply(points_list, function(points_in_cell) {
  st_as_sf(do.call("rbind", points_in_cell), coords = c("X", "Y"), crs = st_crs(yerevan_shapefile))
})

# Combine all the sf objects into a single sf object
points_sf <- do.call("rbind", points_sf_list)

# Save the result as a new shapefile
st_write(points_sf, "points_in_cells.shp", driver = "ESRI Shapefile", crs = st_crs(yerevan_shapefile))





