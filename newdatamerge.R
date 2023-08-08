updatedData <- read.csv("newcleandata.csv")

for(i in 1:nrow(updatedData)) {
  updatedData[i, "lon"] <- str_split(updatedData[i, "WKT"], " ")[[1]][1]  
  updatedData[i, "lat"] <- str_split(updatedData[i, "WKT"], " ")[[1]][2]  
}

updatedData <- updatedData[, -c(1:5)]

updatedData$lon <- as.numeric(updatedData$lon)
updatedData$lat <- as.numeric(updatedData$lat)

clean_data <- clean_data[, -1]

merged_data <- updatedData %>% left_join(clean_data, by = c("lon" = "lon"))
merged_data <- merged_data[, -c(5,6)]

colnames(merged_data) <- c("TYPE", "point", "lon", "lat")

clean_data <- na.omit(clean_data)
write.csv(merged_data, "clean_data.csv")

current_fulldata <- read.csv("totalDistrictData.csv")

View(current_fulldata)

current_fulldata <- current_fulldata[, -c(1:3)]


merged_data <- updatedData %>% left_join(current_fulldata, by = c("lon" = "lon"))
merged_data <- merged_data[, -c(5,8)]
merged_data <- merged_data[, -c(2)]
merged_data <- merged_data[, -c(1)]


merged_data <- updatedData %>% left_join(merged_data, by = c("lon" = "lon"))

merged_data <- merged_data[, -5]
merged_data <- na.omit(merged_data)

write.csv(merged_data, "totalDistrictData.csv")



current_districtInfoData <- read.csv("districtInfoData.csv")

current_districtInfoData <- current_districtInfoData[,-1]

current_districtInfoData$a <- 0
current_districtInfoData$b <- 0
current_districtInfoData$c <- 0

colnames(current_districtInfoData)[2:ncol(current_districtInfoData)] <- unique(merged_data$TYPE)


for(i in 1:nrow(current_districtInfoData)) {
  for(type in unique(merged_data$TYPE)) {
    current_districtInfoData[i, type] <- nrow(merged_data[merged_data$TYPE == type & merged_data$districtName == current_districtInfoData[i, 1],])
  }
}

write.csv(current_districtInfoData, "districtInfoData.csv")


current_cellinfo <- read.csv("cellinfo.csv")
current_cellinfo <- current_cellinfo[, -c(1,2, 3)]



current_cellinfo <- updatedData %>% left_join(current_cellinfo, by = c("lon" = "lon"))
current_cellinfo <- na.omit(current_cellinfo)

write.csv(current_cellinfo, "cellinfo.csv")

current_cellInfoData <- read.csv("cellInfoData.csv")
current_cellInfoData <- current_cellInfoData[, -1]
current_cellInfoData <- as.data.frame(current_cellInfoData[, -c(2:ncol(current_cellInfoData))])

for(type in unique(merged_data$TYPE)) {
  current_cellInfoData[, type] <- 0
}

colnames(current_cellInfoData) <- c("cell", colnames(current_cellInfoData)[2:ncol(current_cellInfoData)])
for(i in 1:nrow(current_cellInfoData)) {
  current_cellInfoData[i, "cell"] <- unique(current_cellinfo$cell)[i]
}


for(i in 1:nrow(current_cellInfoData)) {
  cellID <- current_cellInfoData[i,1]
  numberOfPoints <- 0
  totalCellScore <- 0
  for(varName in colnames(current_cellInfoData)[2:ncol(current_cellInfoData)]) {
    allchecks <- (current_cellinfo$TYPE == varName & current_cellinfo$cell == cellID)
    truechecks <- allchecks[allchecks == TRUE]
    numberOfPoints <- length(truechecks)
    current_cellInfoData[i, varName] <- numberOfPoints
  }
}

for(i in 1:nrow(current_cellInfoData)) {
  totalCellPoints <- 0
  for(varName in colnames(current_cellInfoData)[2:(ncol(current_cellInfoData) - 1)]) {
    totalCellPoints <- totalCellPoints + current_cellInfoData[i, varName]
  }
  current_cellInfoData[i, "cellPoint"] <- totalCellPoints
}


write.csv(current_cellInfoData, "cellInfoData.csv")

cellInfoForBar <- current_cellInfoData
