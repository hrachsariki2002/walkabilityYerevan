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


clean_data <- read.csv("clean_data.csv")

arm <- st_read('Yerevan Districts')
class(arm)
newarm <- as.data.frame(arm)



points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)

mycolor <- st_within(points_sf[1,], arm[1, "geometry"])



pnts_sf <- st_as_sf(points_df, coords = c('lon', 'lat'), crs = st_crs(4326))
pnts_trans <- st_transform(pnts_sf, 2163)
tt_trans <- st_transform(arm, 2163)
pnts_trans <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects( pnts_trans,tt_trans)))

pnts_trans

districtNumbers <- as.data.frame(pnts_trans)
points_sf$district <- distictNumbers$intersection


clean_data$district <- distictNumbers$intersection

districtsmap <- ggplot() + geom_sf(data = arm) +
  geom_sf(data = points_sf, color=clean_data$district)
districtsmap

clean_data$districtName <- newarm$Name.en

clean_data <- clean_data[, -1]


write.csv(clean_data, "points with districts.csv")
