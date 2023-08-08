library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the cleaned data and read the grid shapefile
clean_data <- read.csv("clean_data.csv")
arm <- st_read('Grid_To_Circle')

# Set CRS for the grid
arm <- st_set_crs(arm, 4326)

# Create sf object for the points
points_sf <- st_as_sf(clean_data, coords = c("lon", "lat"), crs = 4326)

# Transform both datasets to have the same CRS
points_sf <- st_transform(points_sf, crs = st_crs(arm))
arm <- st_transform(arm, crs = st_crs(points_sf))

# Perform spatial intersection and assign the district numbers to the points
intersected_cells <- st_intersects(points_sf, arm)

# Create a data frame with row index repeated for each cell number in the list
cell_numbers_df <- data.frame(
  point_id = rep(seq_len(nrow(points_sf)), lengths(intersected_cells)),
  cell_numbers = unlist(intersected_cells)
)

# Merge with points_sf to include cell_numbers as a new column
points_sf <- merge(points_sf, cell_numbers_df, by.x = "row.names", by.y = "point_id", all.x = TRUE)

# Create a new column to store the cell numbers as a list for each point
clean_data$cell <- vector("list", nrow(clean_data))

# Loop through the points and assign the cell numbers to the corresponding points
for (i in seq_len(nrow(cell_numbers_df))) {
  point_index <- cell_numbers_df$point_id[i]
  cell_number <- cell_numbers_df$cell_numbers[i]
  clean_data$cell[[point_index]] <- c(clean_data$cell[[point_index]], cell_number)
}

# Convert the 'cell' column to a character representation
clean_data$cell <- sapply(clean_data$cell, function(x) paste(x, collapse = ","))

# Plot the districts map
districtsmap <- ggplot() + 
  geom_sf(data = arm) +
  geom_sf(data = points_sf, aes(color = factor(cell_numbers)))
districtsmap

# Write the cleaned data to a CSV file
write.csv(clean_data, "circleinfo.csv", row.names = FALSE)
