
server <- function(input, output) {
  
  output$aboutDistrict <- renderText({
    paste("In this case a walkscore has been calculated for each of the Yerevan districts and then based on that an average walkscore for Yerevan as a whole. To calculate the walkscore for each district we have selected random 20 points in each district and then calculated walkscore for each point. The average of those walkscores is the walkscore of the given district. Scorepoints have been given based on the category of the amenity, the distance of that amenity from a given point and the number of amenties from each category within the boundaries of the defined proximity from the given point.")
  })
  
  output$aboutGrid <- renderText({
    paste("In this case a walkscore has been calculated for each of the 35 cell of the 5x7 grid over the entirety of Yerevan and then based on that an average walkscore for Yerevan as a whole. To calculate the walkscore for each cell we have selected random 10 points in each cell and then calculated walkscore for each point. The average of those walkscores is the walkscore of the given cell. Scorepoints have been given based on the category of the amenity, the distance of that amenity from a given point and the number of amenties from each category within the boundaries of the defined proximity from the given point.")
  })
  
  output$map <- renderLeaflet({
    city_map
  })
  
  output$grid_map <- renderLeaflet({
    city_grid_map
  })
  
  output$circle_map <- renderLeaflet({
    city_circle_map
  })
  
  output$averageCircleScore <- renderText({
    paste("Average walking score of Yerevan by circle grid: ", averageCircleWalkScore)
  })
  
  output$averageGridScore <- renderText({
    averageGridWalkScore <- round(sum(newCellInfoData$averagescore) / nrow(newCellInfoData), 2)
    paste("Average walking score of Yerevan by grid: ", averageGridWalkScore)
  })
  
  output$averagescore <- renderText({
    averageWalkScore <- round(sum(districtInfoData$averagescore) / length(districtInfoData$averagescore), 2)
    paste("Average walking score of Yerevan by districts: ", averageWalkScore)
  })
  
  
  output$districtbarplot <- renderPlot({
    districtBarPlot
  })
  
  output$pointProportion <- renderPlot({
    pointProportion
  })
  
  output$cellBarPlot <- renderPlot({
    cellBarPlot
  })
  
  output$cellPointProportion <- renderPlot({
    cellPointProportion
  })  
  
  output$circleBarPlot <- renderPlot({
    circleBarPlot
  })
  
  
  output$circlePointProportion <- renderPlot({
    circlePointProportion
  })  
  
  
  output$categoryBarPlot <- renderPlot({
    neededData <- districtInfoData
    
    
    currentPlot <- ggplot(data = neededData,
                          aes(x = as.factor(District),
                              y = neededData[, input$category])) + geom_bar(stat = "identity")+
      labs(title = paste("The number of ", input$category, " in Yerevan districts"),
           y = paste("Number of ", input$category), x = "Yerevan Districts") +
      theme(axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    currentPlot
  })
  
  
  output$categoryGridBarPlot <- renderPlot({
    neededData <- cellInfoForBar
    
    
    currentPlot <- ggplot(data = neededData,
                          aes(x = as.factor(cell),
                              y = neededData[, input$gridCategory])) + geom_bar(stat = "identity")+
      labs(title = paste("The number of ", input$gridCategory, " in grid cells"),
           y = paste("Number of ", input$gridCategory), x = "Grid Cells") +
      theme(axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    currentPlot
  })
  
  
  observe({
    selected_district <- input$district
    newdata <- fulldata[fulldata$districtName == selected_district, "TYPE"]
    output$districtScore <- renderText({
      if(selected_district == "Nubarashen" | selected_district == "Airport") {
        print("Either the walking score of the chosen district is 0 or there is no data available")
      } else {
        districtInfoText <- " "
        for(i in 2:ncol(districtInfoData)) {
          districtInfoText <- paste(districtInfoText,
                                    colnames(districtInfoData)[i],
                                    ": #",
                                    districtInfoData[districtInfoData$District == selected_district, i])
        }
        
        paste("Walking score of ",
              selected_district, ": ", districtInfoData[districtInfoData$District == selected_district, "averagescore"])        
        
        #        paste("Walking score of ",
        #              selected_district, ": ",
        #              as.vector(fulldata[fulldata$districtName == selected_district, "districtpoints"])[1],
        #              "The following points are present in the district:\n", districtInfoText
        #              )
      }
    })
    
    
    output$districtDataTable <- renderDataTable({
      currentData <- read.csv("districtInfoData.csv")
      currentData <- currentData[,-1]
      
      currentData <- currentData[currentData$District == selected_district,]
      partialData <- currentData[,-c(1, ncol(currentData))]
      
      typeNames <- colnames(partialData)
      typeValues <- c()
      
      for(i in 1:ncol(partialData)) {
        typeValues <- cbind(typeValues, partialData[1, i])
      }
      
      typeValues <- as.vector(typeValues)
      
      readyData <- cbind(typeNames, typeValues)
      readyData <- as.data.frame(readyData)
      colnames(readyData) <- c("Point category", "Number of those points")
      readyData
      
    }, options = list(dom = 't', searching = F))
  })
  
  observe({
    selected_cell <- input$cellOption
    newcelldata <- cellinfo[cellinfo$cell == selected_cell, "TYPE"]
    output$cellScore <- renderText({
      cellInfoText <- " "
      for(i in 2: (ncol(cellInfoData) - 1)) {
        cellInfoText <- paste(cellInfoText,
                              colnames(cellInfoData)[i],
                              ": #",
                              cellInfoData[cellInfoData$cell == selected_cell, i])
      }
      
      paste("Walking score of cell ",
            selected_cell, ": ", newCellInfoData[newCellInfoData$cell == selected_cell, "averagescore"])        
      
      #        paste("Walking score of circle section ",
      #              selected_cell, ": ",
      #              as.vector(cellInfoData[cellInfoData$cell == selected_cell, "cellPoint"])[1],",
      #              The following points are present in the circle section:\n", cellInfoText)
    })
    
    output$gridDataTable <- renderDataTable({
      currentData <- read.csv("cellInfoData.csv")
      currentData <- currentData[,-c(1, ncol(currentData))]
      
      currentData <- currentData[currentData$cell == selected_cell,]
      partialData <- currentData[,-1]
      
      typeNames <- colnames(partialData)
      typeValues <- c()
      
      for(i in 1:ncol(partialData)) {
        typeValues <- cbind(typeValues, partialData[1, i])
      }
      
      typeValues <- as.vector(typeValues)
      
      readyData <- cbind(typeNames, typeValues)
      readyData <- as.data.frame(readyData)     
      colnames(readyData) <- c("Point category", "Number of those points")
      readyData
      
    }, options = list(dom = 't', searching = F))    
    
  })
  
  
  observe({
    selected_circleSection <- input$circleSectionOption
    newcircledata <- circleinfo[circleinfo$cell == selected_circleSection, "TYPE"]
    output$circleSectionScore <- renderText({
      circleInfoText <- " "
      for(i in 5: (ncol(circleInfoData) - 1)) {
        circleInfoText <- paste(circleInfoText,
                                colnames(circleInfoData)[i],
                                ": #",
                                circleInfoData[circleInfoData$cell == selected_circleSection, i])
      }
      
      paste("Walking score of circle section ",
            selected_circleSection, ": ",
            as.vector(circleInfoData[circleInfoData$cell == selected_circleSection, "circlePoint"])[1])
      
      #      paste("Walking score of circle section ",
      #            selected_circleSection, ": ",
      #            as.vector(circleInfoData[circleInfoData$cell == selected_circleSection, "circlePoint"])[1],",
      #              The following points are present in the circle section:\n", circleInfoText)
    })
    
    output$circleDataTable <- renderDataTable({
      currentData <- read.csv("circleInfoData.csv")
      currentData <- currentData[,-c(1, 3, 4, ncol(currentData))]
      
      currentData <- currentData[currentData$cell == selected_circleSection,]
      partialData <- currentData[,-c(1,2)]
      typeNames <- colnames(partialData)
      typeValues <- c()
      
      for(i in 1:ncol(partialData)) {
        typeValues <- cbind(typeValues, partialData[1, i])
      }
      
      typeValues <- as.vector(typeValues)
      
      readyData <- cbind(typeNames, typeValues)
      readyData <- as.data.frame(readyData)
      colnames(readyData) <- c("Point category", "Number of those points")
      readyData
      
    }, options = list(dom = 't', searching = F))
    
  })
  
  
  
  
}