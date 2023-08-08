# Load required packages
library(sp)
library(raster)
library(rgdal)

# Replace "path_to_your_shapefile" with the actual file path of your grid shapefile
grid_shapefile <- readOGR(dsn = "Grid_Yerevan", layer = "grid_yerevan")

# Function to generate random points within a cell
generate_points_in_cell <- function(extent, n_points) {
  x <- runif(n_points, extent[1], extent[2])
  y <- runif(n_points, extent[3], extent[4])
  return(data.frame(lon = x, lat = y))
}

# Initialize an empty dataframe to store the points and cell information
givenpoints_df <- data.frame(lon = numeric(0), lat = numeric(0), cell_number = integer(0))

# Iterate over each cell in the grid
cell_number <- 1
for (i in 1:length(grid_shapefile)) {
  cell_extent <- extent(grid_shapefile[i, ])
  
  # Generate 10 points within the cell
  points_in_cell <- generate_points_in_cell(cell_extent, n_points = 10)
  
  # Add the points to the dataframe along with the cell number
  givenpoints_df <- rbind(givenpoints_df, data.frame(lon = points_in_cell$lon,
                                           lat = points_in_cell$lat,
                                           cell_number = cell_number))
  
  # Increment the cell number
  cell_number <- cell_number + 1
}

# Save the points dataframe to a CSV file
output_csv <- "points_yerevan.csv"
write.csv(givenpoints_df, file = output_csv, row.names = FALSE)
