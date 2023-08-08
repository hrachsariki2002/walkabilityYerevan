library(dplyr)

currentData <- read.csv("districtInfoData.csv")
currentData <- currentData[,-1]

currentData <- currentData[currentData$District == "Ajapnyak",]
partialData <- currentData[,-1]

typeNames <- colnames(partialData)
typeValues <- c()

for(i in 1:ncol(partialData)) {
  typeValues <- cbind(typeValues, partialData[1, i])
}

typeValues <- as.vector(typeValues)

readyData <- cbind(typeNames, typeValues)
readyData <- as.data.frame(readyData)