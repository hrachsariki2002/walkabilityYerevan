# Load required packages
library(sp)
library(raster)
library(rgdal)
library(rgeos) # Load the rgeos package for gBuffer function

# Define the bounding box of Yerevan
min_lon <- 44.362428
max_lon <- 44.60249
min_lat <- 40.063550
max_lat <- 40.23929

# Calculate the number of rows and columns in the grid (4x4)
num_rows <- 3
num_cols <- 4

# Calculate the cell size in decimal degrees
cell_size_lon <- (max_lon - min_lon) / num_cols
cell_size_lat <- (max_lat - min_lat) / num_rows

# Create the extent manually based on the new cell size and grid dimensions
grid_extent <- extent(c(min_lon, max_lon, min_lat, max_lat))

# Create the grid
grid_raster <- raster(grid_extent, nrow = num_rows, ncol = num_cols)

# Convert the raster to a SpatialPoints object representing the center of each cell
grid_points <- as(grid_raster, "SpatialPoints")

# Calculate the buffer radius to create overlapping circles (larger than half of the cell size)
buffer_radius <- min(cell_size_lon, cell_size_lat) * 0.8

# Create circular buffer polygons around each grid point
grid_polygons <- gBuffer(grid_points, width = buffer_radius, byid = TRUE)

# Convert the polygon geometry to a SpatialPolygonsDataFrame
grid_spdf <- SpatialPolygonsDataFrame(grid_polygons, data.frame(ID = 1:length(grid_polygons)))

# Define the output file path and name
output_shapefile <- "grid_yerevan_circular_4x4_overlapping.shp"

# Write the grid to the shapefile
writeOGR(grid_spdf, dsn = ".", layer = "grid_yerevan_circular_4x4_overlapping", driver = "ESRI Shapefile")
