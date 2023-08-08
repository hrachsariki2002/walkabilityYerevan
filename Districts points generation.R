library(sf)
library(ggplot2)

shapefile_folder <- 'Districts'
set.seed(123)

# Get a list of all shapefiles in the folder
shapefile_list <-
  list.files(shapefile_folder, pattern = "\\.shp$", full.names = TRUE)

# Create an empty list to store the sf objects
sf_list <- list()

# Loop through each shapefile and read it using st_read
for (i in seq_along(shapefile_list)) {
  # Extract the file name without the extension to use as the name for the sf object
  file_name <-
    tools::file_path_sans_ext(basename(shapefile_list[i]))
  sf_list[[file_name]] <- st_read(shapefile_list[i], quiet = TRUE)
}

generate_random_points1 <- function(district_sf, n_points) {
  random_points <- st_sample(district_sf, size = n_points, type = "random")
  return(random_points)
}

generate_random_points2 <- function(district_sf, n_points) {
  generated_points <- list()
  district_bbox <- st_bbox(district_sf)
  
  while (length(generated_points) < n_points) {
    random_point <- st_as_sf(
      data.frame(
        x = runif(1, min = district_bbox["xmin"], max = district_bbox["xmax"]),
        y = runif(1, min = district_bbox["ymin"], max = district_bbox["ymax"])
      ),
      coords = c("x", "y"),
      crs = st_crs(district_sf)
    )
    help(st_within)
    if (st_within(random_point, district_sf)) {
      generated_points <- c(generated_points, random_point)
    }
  }
  
  random_points_within_district <- do.call(rbind, generated_points)
  return(random_points_within_district)
}

# Create an empty list to store the point dataframes for each district
point_data_list <- list()

# Loop through each district and generate random points
n_points_per_district <- 20
for (district_name in names(sf_list)) {
  district_sf <- sf_list[[district_name]]
  class(district_sf)
  
  random_points <-
    generate_random_points1(district_sf, n_points_per_district)
  point_data <- st_coordinates(random_points) %>%
    as.data.frame() %>%
    setNames(c("longitude", "latitude"))
  point_data$district <- district_name
  point_data_list[[district_name]] <- point_data
}

# Combine all the point dataframes into a single dataframe
final_point_data <- do.call(rbind, point_data_list)

# Print the resulting dataframe
print(final_point_data)


points_sf <-
  st_as_sf(final_point_data,
           coords = c("longitude", "latitude"),
           crs = 4326)

# Read the Yerevan district shapefile
yerevan_shapefile <-
  st_read('Yerevan Districts/Yerevan-Districts.shp', quiet = TRUE)

# Plot the map
ggplot() +
  geom_sf(data = yerevan_shapefile,
          fill = "lightgray",
          color = "black") +
  geom_sf(data = points_sf,
          color = "red",
          size = 1) +
  labs(title = "Randomly Generated Points in Yerevan Districts") +
  theme_minimal()