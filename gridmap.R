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
library(data.table)

cellinfo <- read.csv("cellinfo.csv")
gridYV <- st_read('Grid_Yerevan')
arm <- st_read('Yerevan Districts')

city_grid_map <- leaflet() %>%
  addTiles() # Add base map tiles (you can also customize the tiles with addProviderTiles())

colors <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')

mypallete <- colorFactor(palette = colors, domain = unique(cellinfo$cell)) 
# cellinfo <- na.omit(cellinfo)

custom_icon <- makeIcon(
  iconUrl = '/Users/hrachkhachatryan/Desktop/ShinyApp\ my\ experiments/pin.png',
  iconWidth = 15,  # Adjust the width if needed
  iconHeight = 15  # Adjust the height if needed
)


city_grid_map <- city_grid_map %>%
  addCircleMarkers(
    data = cellinfo,
    lat = cellinfo$lat,
    lng = cellinfo$lon,
    color = ~mypallete(cellinfo$cell),
    radius = 2,
    opacity = 0.8,
    popup = paste("Lat: ", fulldata$lat, "Lon: ", fulldata$lon, "Type: ", fulldata$TYPE, "Cellid: ", cellinfo$cell)
  ) %>%
  addPolygons(data = gridYV, color = "black", weight = 1.3, fill = F, opacity = 1) %>%
  addMarkers(data = givenpoints_df,
           lng = givenpoints_df$lon,
           lat = givenpoints_df$lat,
           icon = custom_icon,
           popup = paste("Lat: ", givenpoints_df$lon, " Lon: ", givenpoints_df$lat))


city_grid_map

cellBarPlot <- ggplot() + geom_bar(data = cellinfo,
                                       aes(x=as.factor(cell))) +
  labs(x = "Cells of the grid", y = "Number of points in the cell",
       title = "The number of points in each cell",
       subtitle = "5x7 grid over the entirety of Yerevan")


#cellBarPlot


cellPointProportion <- ggplot() +
  geom_bar(data = cellinfo,
           aes(x=as.factor(cell), fill = TYPE), position = "fill") +
  labs(x = "Cells of the grid", y = "Proportion of each category of points in a cell",
       fill = "Point categories",
       title = "Proportion of each category of points for each cell")
#cellPointProportion

averageGridWalkScore <- nrow(cellinfo) * 10 / 35

allcells <- unique(cellInfoData$cell)

# cellInfoData <- cellinfo[, 3:6]
# 
# for(type in unique(cellInfoData$TYPE)) {
#   cellInfoData[, type] <- 0
# }
# cellInfoData <- cellInfoData[1:length(unique(cellInfoData$cell)),]
# 
# 
# for(i in 1:nrow(cellInfoData)) {
#   cellInfoData[i, "cell"] <- unique(cellinfo$cell)[i]
# }
# 
# 
# cellInfoData <- cellInfoData[, 4:length(colnames(cellInfoData))]
# cellInfoData <- na.omit(cellInfoData)
# 
# 
# for(i in 1:nrow(cellInfoData)) {
#   cellID <- cellInfoData[i,1]
#   numberOfPoints <- 0
#   totalCellScore <- 0
#   for(varName in colnames(cellInfoData)[2:ncol(cellInfoData)]) {
#     allchecks <- (cellinfo$TYPE == varName & cellinfo$cell == cellID)
#     truechecks <- allchecks[allchecks == TRUE]
#     numberOfPoints <- length(truechecks)
#     cellInfoData[i, varName] <- numberOfPoints
#   }
# }
# 
# for(i in 1:nrow(cellInfoData)) {
#   totalCellPoints <- 0
#   for(varName in colnames(cellInfoData)[2:(ncol(cellInfoData) - 1)]) {
#     totalCellPoints <- totalCellPoints + cellInfoData[i, varName]
#   }
#   cellInfoData[i, "cellPoint"] <- totalCellPoints * 10
# }
# 
# write.csv(cellInfoData, "cellInfoData.csv")
