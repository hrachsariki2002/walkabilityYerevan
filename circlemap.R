library(rgdal)
library(ggplot2)
library(knitr)
library(dplyr)
library(reticulate)
library(formatR)
library(sf)
library(raster)
library(ggpubr)
library(jpeg)
library(raster)
library(maps)
library(rnaturalearth)
library(rgeos)
library(classInt)
library(ggmap)
library(leaflet)
library(RColorBrewer)



circleYV <- st_read('Grid_To_Circle')
arm <- st_read('Yerevan Districts')


##gridmap <- ggplot() + geom_sf(data=arm) + geom_sf(data=gridYV)

##gridmap

circleinfo <- read.csv("circleinfo.csv")

city_circle_map <- leaflet() %>%
  addTiles() # Add base map tiles (you can also customize the tiles with addProviderTiles())

colors_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
  "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "purple", "#c5b0d5", "#78146b",
  "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5", "black", "#aec7e8", "cyan", "#98df8a",
  "#ff9896", "#c5b0d5", "#c49c94", "green", "blue", "yellow", "#9edae5"
)

mypallete <- colorFactor(palette = colors_vector, domain = unique(circleinfo$cell)) 

city_circle_map <- city_circle_map %>%
  addCircleMarkers(
    data = circleinfo,
    lat = circleinfo$lat,
    lng = circleinfo$lon,
    color = ~mypallete(circleinfo$cell),
    radius = 2,
    opacity = 0.8,
    popup = paste("Lat: ", circleinfo$lat, "Lon: ", circleinfo$lon, "Type: ", circleinfo$TYPE, "Cell: ", circleinfo$cell)
  ) %>%
  addPolygons(data = circleYV, color = "black", weight = 1.3, fill = F, opacity = 1)


city_circle_map



circleBarPlot <- ggplot() + geom_bar(data = circleinfo,
                                   aes(x=as.factor(cell), fill = as.factor(cell))) +
  labs(x = "Circle section", y = "Number of points in the circle section",
       title = "The number of points in each circle section",
       subtitle = "4X3 circle grid over the entirety of Yerevan", fill = "Circle section #") +
      scale_fill_manual(values = colors_vector) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


circleBarPlot

circleInfoData <- circleinfo[, 2:5]

for(type in unique(circleinfo$TYPE)) {
  circleInfoData[, type] <- 0
}


circleInfoData <- circleInfoData[1:length(unique(circleInfoData$cell)),]


for(i in 1:nrow(circleInfoData)) {
  circleInfoData[i, "cell"] <- unique(circleinfo$cell)[i]
}


circleInfoData <- na.omit(circleInfoData)


for(i in 1:nrow(circleInfoData)) {
  cellID <- circleInfoData[i, "cell"]
  numberOfPoints <- 0
  totalCellScore <- 0
  for(varName in colnames(circleInfoData)[5:ncol(circleInfoData)]) {
    allchecks <- (circleinfo$TYPE == varName & circleinfo$cell == cellID)
    truechecks <- allchecks[allchecks == TRUE]
    numberOfPoints <- length(truechecks)
    circleInfoData[i, varName] <- numberOfPoints
  }
}

for(i in 1:nrow(circleInfoData)) {
  totalCirclePoints <- 0
  for(varName in colnames(circleInfoData)[5:(ncol(circleInfoData) - 1)]) {
    #print(i)
    totalCirclePoints <- totalCirclePoints + circleInfoData[i, varName]
    #print(circleInfoData[i, varName])  
  }
  circleInfoData[i, "circlePoint"] <- totalCirclePoints * 10
}

write.csv(circleInfoData, "circleInfoData.csv")


circlePointProportion <- ggplot() +
  geom_bar(data = circleinfo,
           aes(x=as.factor(cell), fill = TYPE), position = "fill") +
  labs(x = "Circle sections", y = "Proportion of each category of points in a circle section",
       fill = "Point categories",
       title = "Proportion of each category of points for each circle section") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
circlePointProportion

averageCircleWalkScore <- nrow(circleinfo) * 10 / length(unique(circleinfo$cell))

allcircles <- unique(circleInfoData$cell)