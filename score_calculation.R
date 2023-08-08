library(geosphere)

# Supermarket/Shop (Food): 20 points
# Place of Worship: 5 points
# School/Kindergarten: 10 points
# Beauty Salon: 5 points
# Bank/ATM: 10 points
# Mall/Shop (Non-food): 5 points
# Bus Stop: 15 points
# Healthcare: 15 points
# Restaurant/Cafe: 10 points
# State Institutions/NGO: 5 points
# Cinema/Theatre/Museum: 5 points
# Pub/Bar: 5 points
# University: 5 points
# Post Office: 5 points

categories <- c(
  "supermarket/shop(food)",
  "place_of_worship",
  "School/kindergarten",
  "beauty_salon",
  "bank/atm",
  "mall/shop(non-food)",
  "bus_stop",
  "healthcare",
  "restaurant/cafe",
  "state institutions/NGO",
  "cinema/theatre/museum",
  "pub/bar",
  "university",
  "post_office"
)


points <- c(20, 5, 10, 5, 10, 5, 15, 15, 10, 5, 5, 5, 5, 5)

categories_and_points <- data.frame(categories, points)

# my_df dataframe where we will keep all our spots' longitude and latitudes and their points for each category

# Initialize the my_df dataframe and set the "lon" and "lat" columns
my_df <- givenpoints_df
for (category in categories) {
  my_df[, category] <- 0
}


main_function <- function(long, lat, rowindex) {
  for (k in 1:nrow(categories_and_points)) {
    # Use seq_along to iterate through indices, not length
    category <- categories[k]
    
    filtered_df <- clean_data[clean_data$TYPE == category, ]
    
    count5min <- 0
    count10min <- 0
    count15min <- 0
    count20min <- 0
    count25min <- 0
    count30min <- 0
    
    for (j in 1:nrow(filtered_df)) {
      # add code for calculating distance "dist" of the given point and the shop or restaurant etc
      
      dist <-
        distGeo(c(long, lat), c(filtered_df[j, "lon"], filtered_df[j, "lat"])) / 1000

      
      if (dist <= 0.5) {
        count5min <- count5min + 1
      } else if (dist <= 1) {
        count10min <- count10min + 1
      } else if (dist <= 1.5) {
        count15min <- count15min + 1
      } else if (dist <= 2) {
        count20min <- count20min + 1
      } else if (dist <= 2.5) {
        count25min <- count25min + 1
      } else if (dist <= 3) {
        count30min <- count30min + 1
      }
    }
    
    # count5min <- 1
    # count10min <- 2
    # count15min <- 4
    # count20min <- 1
    # count25min <- 3
    # count30min <- 2
    # Use if-else statements for comparisons
    if (count5min == 1) {
      my_df[rowindex, category] <- 0.9 * categories_and_points[k, 2]
    } else if (count5min == 2) {
      my_df[rowindex, category] <- 0.95 * categories_and_points[k, 2]
    } else if (count5min >= 3) {
      my_df[rowindex, category] <- 1 * categories_and_points[k, 2]
    } else if (count10min == 1) {
      my_df[rowindex, category] <- 0.8 * 0.9 * categories_and_points[k, 2]
    } else if (count10min == 2) {
      my_df[rowindex, category] <- 0.8 * 0.95 * categories_and_points[k, 2]
    } else if (count10min >= 3) {
      my_df[rowindex, category] <- 0.8 * 1 * categories_and_points[k, 2]
    } else if (count15min == 1) {
      my_df[rowindex, category] <- 0.6 * 0.9 * categories_and_points[k, 2]
    } else if (count15min == 2) {
      my_df[rowindex, category] <- 0.6 * 0.95 * categories_and_points[k, 2]
    } else if (count15min >= 3) {
      my_df[rowindex, category] <- 0.6 * 1 * categories_and_points[k, 2]
    } else if (count20min == 1) {
      my_df[rowindex, category] <- 0.5 * 0.9 * categories_and_points[k, 2]
    } else if (count20min == 2) {
      my_df[rowindex, category] <- 0.5 * 0.95 * categories_and_points[k, 2]
    } else if (count20min >= 3) {
      my_df[rowindex, category] <- 0.5 * 1 * categories_and_points[k, 2]
    } else if (count25min == 1) {
      my_df[rowindex, category] <- 1 * 0.9 * categories_and_points[k, 2]
    } else if (count25min == 2) {
      my_df[rowindex, category] <- 1 * 0.95 * categories_and_points[k, 2]
    } else if (count25min >= 3) {
      my_df[rowindex, category] <- 1 * 1 * categories_and_points[k, 2]
    } else if (count30min == 1) {
      my_df[rowindex, category] <- 1 * 0.9 * categories_and_points[k, 2]
    }
  }
  
  return(my_df)
}

for(i in 1:nrow(my_df)) {
  my_df <- main_function(my_df[i, "lon"], my_df[i, "lat"], i)
}

my_df$totalScore <- 0
for(i in 1:nrow(my_df)) {
  currentScore <- 0
  for(category in colnames(my_df)[4:ncol(my_df)]) {
    currentScore <- currentScore + my_df[i, category]
  }
  my_df[i, "totalScore"] <- currentScore
}

newCellInfoData <- data.frame(cbind(unique(my_df$cell), unique(my_df$cell)))
colnames(newCellInfoData) <- c("cell", "val")
for(cell in unique(my_df$cell)) {
  allpoints <- my_df[my_df$cell == cell, "totalScore"]
  newCellInfoData[newCellInfoData$cell == cell, "averagescore"] <- sum(allpoints) / length(allpoints)
}
newCellInfoData$val <- NULL

write.csv(my_df, "gridWalkScore.csv")
write.csv(newCellInfoData, "newCellInfoData.csv")
