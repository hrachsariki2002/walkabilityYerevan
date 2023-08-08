# Load required packages
library(sp)
library(raster)
library(rgdal)

# Define the bounding box of Yerevan
min_lon <- 44.362428
max_lon <- 44.60249
min_lat <- 40.063550
max_lat <- 40.23929

# Create the extent manually
grid_extent <- extent(c(min_lon, max_lon, min_lat, max_lat))

# Define the cell size in decimal degrees
cell_size <- 0.035  # Approximately 100 meters

# Create the grid
grid_raster <- raster(grid_extent, res = cell_size)

# Convert the raster to a SpatialPolygonsDataFrame
grid_spdf <- as(grid_raster, "SpatialPolygonsDataFrame")

# Define the output file path and name
output_shapefile <- "grid_yerevan.shp"

# Write the grid to the shapefile
writeOGR(grid_spdf, dsn = ".", layer = "grid_yerevan", driver = "ESRI Shapefile")
