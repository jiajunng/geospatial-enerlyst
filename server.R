
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

library(maptools)
library(rgdal)
library(leaflet)
shpListName <-list()
shpList<-list()
shinyServer(function(input, output) {
  in_data <- reactive({
    shpListName<<-c(shpListName,gsub("\\..*","",input$inputdata$name[1]))
  })
 
  observe({
    if(is.null(input$selectedShp))
      return()
    testing<-input$selectedShp
    
    asd<-match(testing,shpListName)
    shptest<-shpList[asd]
    print(shptest)
    test1 <- do.call(rbind, lapply(shptest, data.frame, stringsAsFactors=FALSE))
    coordinates(test1) <-~XCOORD+YCOORD
    proj4string(test1) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    shapeData <- spTransform(test1, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    # plot(test1)
    
    leafletProxy("mymap") %>%
      addMarkers(data = shapeData)
    
  })
  observe({
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
    
    
    # Check boxes
    output$choose <- renderUI({
      # If missing input, return to avoid error later in function
      if(is.null(input$dataset))
        return()
      
      checkboxGroupInput("selectedShp", "Choose Layer", 
                         choices  = in_data(),
                         selected = 1)
      
      
    })
  })
  output$value <- renderPlot({ 
    selectedShp=input$selectedShp
    shpListName
    test<-shpList[1]
    test1 <- do.call(rbind, lapply(test, data.frame, stringsAsFactors=FALSE))
    coordinates(test1) <-~XCOORD+YCOORD
    plot(test1)
    })
  
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)%>%
      addTiles()
  })
  observe({
    
    if(input$dataset=="cars"){
      test<-shpList[1]
      test1 <- do.call(rbind, lapply(test, data.frame, stringsAsFactors=FALSE))
      coordinates(test1) <-~XCOORD+YCOORD
      proj4string(test1) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
      shapeData <- spTransform(test1, CRS("+proj=longlat +datum=WGS84 +no_defs"))
      # plot(test1)
      
      leafletProxy("mymap") %>%
        addMarkers(data = shapeData)
    }
    
  })
  
})
