ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #FDFDFC;
    }
  ")),
  tabsetPanel(
    tabPanel(title= "By Districts",
             sidebarLayout(
               sidebarPanel(div(textOutput("averagescore"), style = "padding: 0 0 25px 0; border-bottom: 1px solid #ddd; font-weight: bold;"),
                            div(selectInput("district", "Choose a district", choices = alldistricts, selected = "Kentron"), textOutput("districtScore"),
                                style="padding: 25px 0; border-bottom: 1px solid #ddd;"),
                            
                            div(selectInput("category", "Choose a category", choices = colnames(districtInfoData)[2:ncol(districtInfoData)]),
                                style="padding: 25px 0 15px 0; border-bottom: 1px solid #ddd;"),
                            div(textOutput("aboutDistrict"), style="padding: 25px 0 0 0;")
                            ),
               mainPanel(
                 div(leafletOutput("map"),
                     style="padding: 25px; border-left: 1px solid #ddd; background-color: #fff;"),
                 div(plotOutput("categoryBarPlot"),
                     style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),
                 div(dataTableOutput("districtDataTable"), style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),
                 div(plotOutput("districtbarplot"),
                     style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),
                 div(plotOutput("pointProportion"),
                     style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;")
                 
                 
               )
             )
             
    ),
    tabPanel(title = "By grid",
             sidebarLayout(
               
               sidebarPanel(div(textOutput("averageGridScore"), style = "padding: 0 0 25px 0; border-bottom: 1px solid #ddd; font-weight: bold;"),
                            div(selectInput("cellOption", "Choose a cell", choices = unique(newCellInfoData$cell)), textOutput("cellScore"),
                                style="padding: 25px 0; border-bottom: 1px solid #ddd;"),
                            
                            div(selectInput("gridCategory", "Choose a category", choices = colnames(cellInfoForBar[,3:(ncol(cellInfoForBar) - 1)])),
                                style="padding: 25px 0 15px 0; border-bottom: 1px solid #ddd;"),
                            div(textOutput("aboutGrid"), style="padding: 25px 0 0 0;")
               ),
               mainPanel(div(leafletOutput("grid_map"),
                             style="padding: 25px; border-left: 1px solid #ddd; background-color: #fff;"),
                         div(plotOutput("categoryGridBarPlot"),
                             style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),              
                         div(dataTableOutput("gridDataTable"), style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),              
                         div(plotOutput("cellBarPlot"),
                             style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),
                         div(plotOutput("cellPointProportion"),
                             style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;")
                         
               )
             )
    )
    # tabPanel(title = "By radius",
    #          sidebarLayout(
    #            sidebarPanel(textOutput("averageCircleScore"),
    #                         selectInput("circleSectionOption", "Choose a circle section", choices =  allcircles),
    #                         textOutput("circleSectionScore")),
    #            mainPanel(
    #              div(leafletOutput("circle_map"),
    #               style="padding: 25px; border-left: 1px solid #ddd; background-color: #fff;"),
    #              div(plotOutput("circleBarPlot"), style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),
    #              div(plotOutput("circlePointProportion"), style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;"),
    #              div(dataTableOutput("circleDataTable"), style = "padding: 25px; border-top: 1px solid #ddd; border-left: 1px solid #ddd; background-color: #fff;")
    #            )
    #          )
    # ),
    # tabPanel(title = "Where do people walk?")
  )
)
