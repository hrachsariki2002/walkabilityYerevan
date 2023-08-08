library(ggplot2) 
library(ggmap)
library(plotly)
library(dplyr)
library(rstudioapi)
library(stringr)
library(leaflet)
library(sf)
library(RColorBrewer)


clean_data <- read.csv("clean_data.csv")
fulldata <- read.csv("points with districts.csv")
yv_districts <- st_read('Yerevan Districts')
# districts <- read.csv("districts.csv")
# fulldata <- na.omit(fulldata)

# fulldata$districtName <- "Hello"
# 
# fulldata[fulldata$cell == "11", "districtName"] = "Erebuni"
# fulldata[fulldata$cell == "8", "districtName"] = "Shengavit"
# fulldata[fulldata$cell == "6", "districtName"] = "Malatia-Sebastia"
# fulldata[fulldata$cell == "5", "districtName"] = "Davtashen"
# fulldata[fulldata$cell == "4", "districtName"] = "Arabkir"
# fulldata[fulldata$cell == "3", "districtName"] = "Kanaker-Zeytun"
# fulldata[fulldata$cell == "1", "districtName"] = "Avan"
# fulldata[fulldata$cell == "2", "districtName"] = "Nor Nork"
# fulldata[fulldata$cell == "10", "districtName"] = "Nork-Marash"
# fulldata[fulldata$cell == "9", "districtName"] = "Kentron"
# fulldata[fulldata$cell == "13", "districtName"] = "Ajapnyak"
# fulldata$districtpoints <- 0

# write.csv(fulldata, "totalDistrictData.csv")
# erebunipoints <- 0
# 
totalScore <- 0

for(district in unique(fulldata$districtName)) {
  districtpoint <- 0
  for(i in (fulldata$districtName == district)) {
    if(i == T) {
      districtpoint <- districtpoint + 10
    }
  }
  totalScore <- totalScore + districtpoint
#  fulldata[fulldata$districtName == district, "districtpoints"] <- districtpoint
}


alldistricts <- c("Nubarashen", "Airport", unique(fulldata$districtName))

averageWalkScore <- totalScore / length(alldistricts)
  


#points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)
full_sf <- st_as_sf(fulldata, coords = c("lon", "lat"), crs = 4326)
city_map <- leaflet() %>%
  addTiles() # Add base map tiles (you can also customize the tiles with addProviderTiles())

finaldata <- read.csv("totalDistrictData.csv")

mypallete <- colorFactor(palette = c(brewer.pal(9, "Blues"), "cyan","purple", "pink", "blue"), domain = c(1:13, NA)) 

custom_icon <- makeIcon(
  iconUrl = '/Users/hrachkhachatryan/Desktop/ShinyApp\ my\ experiments/pin.png',
  iconWidth = 20,  # Adjust the width if needed
  iconHeight = 20  # Adjust the height if needed
)

city_map <- city_map %>%
  addCircleMarkers(
    data = finaldata,
    lat = finaldata$lat,
    lng = finaldata$lon,
    color = ~mypallete(finaldata$cell),
    radius = 2,
    opacity = 0.8,
    popup = paste("Lat: ", finaldata$lat, "Lon: ", finaldata$lon, "Type: ", finaldata$TYPE)
  ) %>%
  addPolygons(data = yv_districts, color = "black", weight = 3, fill = F, opacity = 1) %>%
  addMarkers(data = final_point_data,
    lng = final_point_data$longitude,
             lat = final_point_data$latitude,
             icon = custom_icon,
             popup = paste("Lat: ", final_point_data$latitude, " Lon: ", final_point_data$longitude))


city_map


districtBarPlot <- ggplot() + geom_bar(data = fulldata,
                                       aes(x=districtName)) +
  labs(x = "Yerevan Districts", y = "Number of points in the district",
       title = "The number of points in each Yerevan District",
       subtitle = "Except Nubarashen and Airport") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#districtBarPlot


pointProportion <- ggplot() +
  geom_bar(data = fulldata,
           aes(x=districtName, fill = TYPE), position = "fill") +
  labs(x = "Yerevan Districts (abv)", y = "Proportion of each category of points in a district",
       fill = "Point categories",
       title = "Proportion of each category of points for each Yerevan district") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#pointProportion


districtInfoData <- read.csv("districtInfoData.csv")
districtInfoData <- districtInfoData[,-1]
# districtInfoData <- as.data.frame(cbind(unique(fulldata$districtName), c(1:11)))
# colnames(districtInfoData) <- c("District", "idk")
# for(i in unique(fulldata$districtName)) {
#   districtInfoData <- cbind(districtInfoData, c(1:11))
#   
# }

# districtInfoData <- districtInfoData[,-2]
# colnames(districtInfoData) <- c("District", unique(fulldata$TYPE))

# 
# for(i in 1:nrow(districtInfoData)) {
#   for(type in unique(fulldata$TYPE)) {
#     districtInfoData[i, type] <- nrow(fulldata[fulldata$TYPE == type & fulldata$districtName == districtInfoData[i, 1],])
#   }
# }
# 
# write.csv(districtInfoData, "districtInfoData.csv")
