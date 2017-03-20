
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(maptools)
library(rgdal)
shpListName <-list()
shpList<-list()
shinyServer(function(input, output) {
  in_data <- reactive({
    shpListName<<-c(shpListName,gsub("\\..*","",input$inputdata$name[1]))
  })
  # in_data1 <- reactive({
  # 
  #   myshape<- input$inputdata
  #   if (is.null(myshape))
  #     return(NULL)
  #   dir<-dirname(myshape[1,4])
  #   for ( i in 1:nrow(myshape)) {
  #     file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
  #   getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
  #   shape<-readOGR(dsn = getshp)
  #   shpList<<-c(shpList,shape)
  #   shpList<<-c(shpList,shape)
  # })
  # values <- reactiveValues()
 
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    # y <- input$layer
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    myshape<- input$inputdata
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    shape<-readOGR(dsn = getshp)
     shpList<<-c(shpList,shape)
     print(length(shpList))
     test<-shpList[1]
     test1 <- do.call(rbind, lapply(test, data.frame, stringsAsFactors=FALSE))
     coordinates(test1) <-~XCOORD+YCOORD
     
     #test=shpList[,1]
    plot(test1)
    # plot (l$your.x.coordinate, l$your.y.coordinate)
    output$text<-renderText({
     
      "testdsdsd"
      
    })
    # Check boxes
    output$choose <- renderUI({
      # If missing input, return to avoid error later in function
      if(is.null(input$dataset))
        return()
      
      # Get the data set with the appropriate name
      # dat <- get(input$dataset)
      # colnames <- names(dat)
      
       #c(in_data, 7)
      # Create the checkboxes and select them all by default
      checkboxGroupInput("selectedShp", "Choose columns", 
                         choices  = in_data(),
                         selected = 1)
    })
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  output$value <- renderPlot({ 
    selectedShp=input$selectedShp
    shpListName
    test<-shpList[1]
    test1 <- do.call(rbind, lapply(test, data.frame, stringsAsFactors=FALSE))
    coordinates(test1) <-~XCOORD+YCOORD
    plot(test1)
    })
  output$text <- renderText({
    # ab=input$checkGroup
    # ab=input$checkGroup
  
  })
})
