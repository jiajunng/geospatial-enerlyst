
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(maptools)
library(rgdal)
library(leaflet)
library(spatialEco)
library(plyr)
library(spdep)
library(GISTools)
library(spatstat)
library(classInt)
library(RColorBrewer)
library(rsconnect)
# rsconnect::setAccountInfo(name='onthefly',
#                           token='680ABC7B5A35A89CC84375F50746490A',
#                           secret='6CFTy6unuRUq0FwOsys9Ndx225l39gzlF8cqUUZP')
# rsconnect::deployApp()
# deployApp()
month <- 0
shpListName <-list()
shpList<-list()
ecList<-list()
subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL")

shinyServer(function(input, output) {
  in_data <- reactive({
    shpListName<<-c(shpListName,gsub("\\..*","",input$inputdata$name[1]))
  })
  
  observe({
    #print(input$monthSelector)
    if (length(ecList)==0) {
      #subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL") # read shapefile
      
      # read private housing energy consumption data
      privEC <- read.csv ("data/private/private_housing.csv") # read csv
      coordinates(privEC) <- ~X+Y
      # convert files to same CRS
      proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
      proj4string(privEC) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
      
      # use spatiaEco to map houses to subzones
      privEC_in_subzone <- point.in.poly(privEC, subzones)
      privEC_in_subzone_df <- as.data.frame(privEC_in_subzone)
      
      aggregated_avg_priv_ec_by_subzone <- aggregateECbyMonthBySubzone(privEC_in_subzone_df, subzones)
      
      ecList[["2015"]] <<- aggregated_avg_priv_ec_by_subzone
      # ecList<<-c(shpList,aggregated_avg_priv_ec_by_subzone)
    }
    
    monthName <- switch(input$monthSelector, "1" = "Jan", "2" = "Feb", "3" = "Mar",
                        "4" = "Apr", "5" = "May", "6" = "Jun", "7"="Jul","8"="Aug",
                        "9"="Sep","10"="Oct","11"="Nov","12"="Dec")
    output$title <- renderUI({
      h3(paste("Energy Consumption in ", monthName))
    })
    
    #get selected year's data
    aggregated_avg_priv_ec_by_subzone<-ecList[["2015"]] 
    aggregated_avg_priv_ec_by_subzone_noNA <- completeFun(aggregated_avg_priv_ec_by_subzone, monthName)
    
    # dont include rows with 0s
    aggregated_avg_priv_ec_by_subzone_noNA<-subset(aggregated_avg_priv_ec_by_subzone_noNA, aggregated_avg_priv_ec_by_subzone_noNA[ , monthName] > 0) 
    
    subzones_noNA <- merge(subzones, aggregated_avg_priv_ec_by_subzone_noNA, by.x="SUBZONE_N",
                           by.y="SUBZONE_N", all.x=FALSE)
    
    wm_q <- poly2nb(subzones_noNA, queen=TRUE)
    rswm_q <- nb2listw(wm_q, zero.policy = TRUE)
    
    aggregated_avg_priv_ec_by_subzone[is.na(aggregated_avg_priv_ec_by_subzone)] <- 0
    # localMI <- localmoran(aggregated_avg_priv_ec_by_subzone_noNA$monthNames, rswm_q)
    localMI <- localmoran(aggregated_avg_priv_ec_by_subzone_noNA[,c(monthName)], rswm_q)
    
    
    #localMI[is.na(localMI)] <- 0 #replace Na with 0
    localMI <- na.omit(localMI) #straight out remove rows with NA
    
    
    quadrant <- vector(mode="numeric",length=nrow(localMI))
    DV <- aggregated_avg_priv_ec_by_subzone_noNA[,c(monthName)] - mean(aggregated_avg_priv_ec_by_subzone_noNA[,c(monthName)])
    
    C_mI <- localMI[,1] - mean(localMI[,1])
  
    signif <- 0.1
    quadrant[DV >0 & C_mI>0] <- 4
    quadrant[DV <0 & C_mI<0] <- 1
    quadrant[DV <0 & C_mI>0] <- 2
    quadrant[DV >0 & C_mI<0] <- 3
    quadrant[localMI[,5]>signif] <- 0
    #print(quadrant)
    brks <- c(0,1,2,3,4)
    colors <- c("#ffffff","#0571b0","#92c5de","#f4a582","red")
    
    proj4string(subzones_noNA) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
     shapeData <<- spTransform(subzones_noNA, CRS("+proj=longlat +datum=WGS84 +no_defs"))
     
     # plotChoro()
     
     ###############################NEW
     # colors and classes for choropleth
     # pcol.C = brewer.pal(5,"Blues")
     # frame.C <- as.data.frame(shapeData[,c(monthName)])
     # nclass.C =classIntervals(frame.C[,1], n=5, style='pretty', intervalClosure='right')
     # colcode.C = findColours(nclass.C, pcol.C)
     # #qpal.C <- colorQuantile("Blues", frame.C[,1], n = 6) — returns %
     # pal.C<- colorBin(palette = "Blues", domain = frame.C[,1], bins=8)
     
     # colors and classes for local moran
     pcol.LMI= brewer.pal(5,"PRGn")
     nclass.LMI =classIntervals(localMI[,1], n=5, intervalClosure='right')
     colcode.LMI = findColours(nclass.LMI, pcol.LMI)
     #qpal.LMI <- colorQuantile("PRGn", localMI[,1], n = 5)
     pal.LMI<- colorBin(palette = "PRGn", domain = localMI[,1], bins=4)
     toListen <- reactive({
       list(input$mymap_groups,input$choroColor,input$choroClass,input$choroMethod)
     })
     
     # leaflet with layers
     observeEvent(
       toListen(),{
       leafletProxy("mymap", data = shapeData) %>%
         clearShapes() %>%
         clearControls() %>%
         addTiles()
       
       #print(input$mymap_groups[2])
       if(is.null(input$mymap_groups)){
         return()
       }
       
       if (grepl("Choropleth", input$mymap_groups[1])) {
         showElement("choroClass", anim = TRUE)
         showElement("choroColor", anim = TRUE)
         showElement("choroMethod", anim = TRUE)
         monthName <- switch(input$monthSelector, "1" = "Jan", "2" = "Feb", "3" = "Mar",
                             "4" = "Apr", "5" = "May", "6" = "Jun", "7"="Jul","8"="Aug",
                             "9"="Sep","10"="Oct","11"="Nov","12"="Dec")
         #print(paste("choro for",monthName))
         # pcol.C = rev(brewer.pal(input$choroClass,input$choroColor))
         pcol.C = brewer.pal(input$choroClass,input$choroColor)
         frame.C <- as.data.frame(shapeData[,c(monthName)])
         nclass.C =classIntervals(frame.C[,1], n=input$choroClass, style=input$choroMethod,                         intervalClosure='right')
         colcode.C = findColours(nclass.C, pcol.C)
         
         pal_volume<-colorBin(palette = input$choroColor, domain = frame.C[,1], bins =input$choroClass, pretty = FALSE)
         
         qpal.C <- colorQuantile(input$choroColor, frame.C[,1], n = 6)
         pal.C<- colorBin(palette = input$choroColor, domain = frame.C[,1], bins = input$choroClass)
         print(input$choroClass)
         print(colcode.C)
         testing<- as.data.frame(shapeData)
         #clear first before adding new layers
         leafletProxy("mymap") %>%
           clearGroup(group="Choropleth")
         
         # popup <-paste(paste0("<b>SUBZONE:</b> ",shapeData$SUBZONE_N), paste0("<b>AVG Energy Consumption: </b>",frame.C[,1]), sep="<br/>")
         popup <-paste(paste0("<h4><b>",shapeData$SUBZONE_N, "</b></h4>"), paste0("<b>AVG Energy Consumption: </b>",format(round(frame.C[,1], 2), nsmall = 2), " kWh"), sep="<br/>")
         leafletProxy("mymap") %>%
           addTiles() %>% 
           addPolygons(data = shapeData, color = "black", fillColor = colcode.C, weight=1,
                       fillOpacity=0.7, group="Choropleth", popup = popup,
                       highlightOptions = highlightOptions(color = "white", weight = 3,bringToFront = TRUE)) %>%
           addLegend(pal = pal_volume, values = frame.C[,1],
                     opacity = 0.7, title="Avg Energy Consumption")
         
       }
       else if (grepl("LISA", input$mymap_groups[1])) {
         hideChoroControls()
         leafletProxy("mymap", data = shapeData) %>%
           addPolygons(color = "black",
                       fillColor = colors[findInterval(quadrant,brks,all.inside=FALSE)],
                       fillOpacity=0.8, weight=1, group="LISA") %>%
           addLegend(labels=c("Insignificant","Low-Low","Low-High","High-Low","High-High"),
                     colors=colors,
                     values = frame.C[,1], opacity = 0.7, title="LISA")
       }
       else if (grepl("Local Moran I", input$mymap_groups[1])) {
         hideChoroControls()
         leafletProxy("mymap", data = shapeData) %>%
           addPolygons(color = "black", fillColor = colcode.LMI ,fillOpacity=0.8,
                       weight = 1, group = "Local Moran I")%>%
           addLegend(pal=pal.LMI, values = localMI[,1], 
                     opacity = 0.7, title = "Local Moran I")
       }
     })
     ###############################NEW END
     
     # pcol.LMI= brewer.pal(5,"PRGn")
     # nclass.LMI =classIntervals(localMI[,1], n=6, intervalClosure='right')
     # colcode.LMI = findColours(nclass.LMI, pcol.LMI)
     # 
     # #clear first before adding new layers
     # leafletProxy("mymap") %>%
     #   clearGroup(group="LISA")%>%
     #   clearGroup(group="Local Moran I")
     # 
     # leafletProxy("mymap") %>%
     #   addTiles() %>% 
     #   addPolygons(data = shapeData, color = "black", fillOpacity=0.5, weight=1,
     #               fillColor = colors[findInterval(quadrant,brks,all.inside=FALSE)], 
     #               group="LISA") %>%
     #  addPolygons(data = shapeData, color = "black", weight=1,
     #              fillColor = colcode.LMI, fillOpacity = 0.7, group = "Local Moran I")
      
    
    
    output$MS <- renderPlot({
      moran.plot(aggregated_avg_priv_ec_by_subzone_noNA[,c(monthName)], rswm_q, zero.policy = TRUE, spChk=FALSE, labels=as.character(aggregated_avg_priv_ec_by_subzone_noNA$SUBZONE_N), xlab="Average Energy Consumption", ylab="Spatially Lag Average Energy Consumption")
    })
     print("END")
  })
  
  # plotChoro<-reactive({
  #   monthName <- switch(input$monthSelector, "1" = "Jan", "2" = "Feb", "3" = "Mar",
  #                        "4" = "Apr", "5" = "May", "6" = "Jun", "7"="Jul","8"="Aug",
  #                        "9"="Sep","10"="Oct","11"="Nov","12"="Dec")
  #   #print(paste("choro for",monthName))
  #   # pcol.C = rev(brewer.pal(input$choroClass,input$choroColor))
  #   pcol.C = brewer.pal(input$choroClass,input$choroColor)
  #   frame.C <- as.data.frame(shapeData[,c(monthName)])
  #   nclass.C =classIntervals(frame.C[,1], n=input$choroClass, style=input$choroMethod,                         intervalClosure='right')
  #   colcode.C = findColours(nclass.C, pcol.C)
  #   qpal.C <- colorQuantile(input$choroColor, frame.C[,1], n = 6)
  #   testing<- as.data.frame(shapeData)
  #   #clear first before adding new layers
  #   leafletProxy("mymap") %>%
  #     clearGroup(group="Choropleth")
  # 
  #   # popup <-paste(paste0("<b>SUBZONE:</b> ",shapeData$SUBZONE_N), paste0("<b>AVG Energy Consumption: </b>",frame.C[,1]), sep="<br/>")
  #   popup <-paste(paste0("<h4><b>",shapeData$SUBZONE_N, "</b></h4>"), paste0("<b>AVG Energy Consumption: </b>",format(round(frame.C[,1], 2), nsmall = 2), " kWh"), sep="<br/>")
  #   leafletProxy("mymap") %>%
  #     addTiles() %>%
  #     addPolygons(data = shapeData, color = "black", fillColor = colcode.C, weight=1,
  #                 fillOpacity=0.7, group="Choropleth", popup = popup,
  #                 highlightOptions = highlightOptions(color = "white", weight = 3,bringToFront = TRUE)) %>%
  #     addLegend(pal = qpal.C, values = frame.C[,1], opacity = 1, labels=nclass.C)
  # })
  
  
  observeEvent(input$togglePlot, {
    toggle("MS")
  })
  
  observe({
    print(input$selectedShp)
    if(is.null(input$selectedShp))
    {
      clearMarkers(leafletProxy("mymap"))
      return()
    }
      
    testing<-input$selectedShp
    
    asd<-match(testing,shpListName)
    shptest<-shpList[asd]
    print(shptest)
    test1 <- do.call(rbind, lapply(shptest, data.frame, stringsAsFactors=FALSE))
    coordinates(test1) <-~XCOORD+YCOORD
    proj4string(test1) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    shapeData <- spTransform(test1, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    # removeShape(map, layerId)
    
    leafletProxy("mymap") %>%
     
      addMarkers(data = shapeData)
      
  })

  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  hideChoroControls <- function()
  {
    hide("choroClass", anim = TRUE)
    hide("choroColor", anim = TRUE)
    hide("choroMethod", anim = TRUE)
  }
  aggregateECbyMonthBySubzone <- function(privEC_in_subzone_df, subzones){
    for (month in c(1,2,3,4,5,6,7,8,9,10,11,12)){
      privEC_in_subzone_df[, month+1] <- as.numeric(as.character(privEC_in_subzone_df[, month+1] ))
      privEC_in_subzone_df[is.na(privEC_in_subzone_df)] <- 0
      aggregate_ec_by_subzone <- aggregate(privEC_in_subzone_df[,month+1], by=list(SUBZONE_N=privEC_in_subzone_df$SUBZONE_N), FUN=sum)
      count_housings_in_subzone <- aggregate(privEC_in_subzone_df[,month+1], by=list(SUBZONE_N=privEC_in_subzone_df$SUBZONE_N), FUN=function(x){NROW(x)})
      #print(count)
     
      # merge aggregate of energy consumption for Jan to subzone
      subzones <- merge(subzones, aggregate_ec_by_subzone, by.x="SUBZONE_N", by.y="SUBZONE_N", all.x=TRUE)
      names(subzones)[names(subzones)=="x"] <- "sum"
      
      subzones <- merge(subzones, count_housings_in_subzone, by.x="SUBZONE_N", by.y="SUBZONE_N", all.x=TRUE)
      names(subzones)[names(subzones)=="x"] <- "count"
      
      # get energy consumption in Jan per km square of subzone
      # subzones <- transform(subzones, avg = x / (SHAPE_Area / 1000000))
      subzones <- transform(subzones, avg = sum / count)
      #rename(merged, c("avg"="montesh"))
      
      monthNames <- switch(month, "1" = "Jan", "2" = "Feb", "3" = "Mar","4" = "Apr", "5" = "May",
             "6" = "Jun", "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")
      
      names(subzones)[names(subzones)=="avg"] <- monthNames
      
      #drop the x column since incoming iteration would be using the same name
      subzones<-subset(subzones, select=-c(sum))
      subzones<-subset(subzones, select=-c(count))
    }
    
    #aggregate_ec_by_subzone <- aggregate(privEC_in_subzone_df$Jan, by=list(SUBZONE_N=privEC_in_subzone_df$SUBZONE_N), FUN=sum)
    return(subzones)
  }
  
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
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)%>%
      # addProviderTiles(providers$CartoDB.Positron, group = "Positron")%>%
      # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addLayersControl(
        #overlayGroups = c("OSM (default)", "Positron", "Toner Lite"),
        baseGroups = c("Choropleth", "LISA", "Local Moran I"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      )%>%
      hideGroup("Positron")%>% hideGroup("Toner Lite")
  })
  
  
  
})
