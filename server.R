
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
fileList <- list()
fileNameList <- list()
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
      preloadData()
    }
    
    monthName <- switch(input$monthSelector, "1" = "Jan", "2" = "Feb", "3" = "Mar",
                        "4" = "Apr", "5" = "May", "6" = "Jun", "7"="Jul","8"="Aug",
                        "9"="Sep","10"="Oct","11"="Nov","12"="Dec")
   
    #get selected year's data
    # yearSelected <- input$yearSelector
    # unlist(input$yearSelector, "[-]")
    if(input$dataType == "Private")
    {
      yearSelected <- paste(paste(input$yearSelector,"_"), "priv")
    }else if(input$dataType == "Public")
    {
      yearSelected <- paste(paste(input$yearSelector,"_"), "pub")
    }
    else
    {
      yearSelected <- paste(paste(input$yearSelector,"_"), "combined")
    }
    yearSelected <- gsub(" ", "", yearSelected, fixed = TRUE)
    
    #Output Title
    output$title <- renderUI({
      h4(paste(paste("Energy Consumption in ", monthName), paste(" for year", yearSelected)))
    })
    
    selected_ec_by_subzone<-ecList[[yearSelected]] 
    selected_ec_by_subzone_noNA <- completeFun(selected_ec_by_subzone, monthName)
    
    # dont include rows with 0s
    selected_ec_by_subzone<-subset(selected_ec_by_subzone, selected_ec_by_subzone[ , monthName] > 0) 
    
    subzones_noNA <- merge(subzones, selected_ec_by_subzone, by.x="SUBZONE_N",
                           by.y="SUBZONE_N", all.x=FALSE)
    
    wm_q <- poly2nb(subzones_noNA, queen=TRUE)
    rswm_q <- nb2listw(wm_q, zero.policy = TRUE)
    
    selected_ec_by_subzone[is.na(selected_ec_by_subzone)] <- 0
    # localMI <- localmoran(aggregated_avg_priv_ec_by_subzone_noNA$monthNames, rswm_q)
    localMI <- localmoran(selected_ec_by_subzone[,c(monthName)], rswm_q)
    
    #localMI[is.na(localMI)] <- 0 #replace Na with 0
    localMI <- na.omit(localMI) #straight out remove rows with NA
    
    quadrant <- vector(mode="numeric",length=nrow(localMI))
    DV <- selected_ec_by_subzone[,c(monthName)] - mean(selected_ec_by_subzone[,c(monthName)])
    
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
     
     
     # colors and classes for local moran
     pcol.LMI= brewer.pal(5,"PRGn")
     nclass.LMI =classIntervals(localMI[,1], n=5, intervalClosure='right')
     colcode.LMI = findColours(nclass.LMI, pcol.LMI)
     #qpal.LMI <- colorQuantile("PRGn", localMI[,1], n = 5)
     pal.LMI<- colorBin(palette = "PRGn", domain = localMI[,1], bins=4)
     
     toListen <- reactive({
       list(input$mymap_groups,input$choroColor,input$choroClass,input$choroMethod)
     })
     
     # leaflet with layers change 
     observeEvent(
       toListen(),{
       leafletProxy("mymap", data = shapeData) %>%
         clearShapes() %>%
         clearControls() %>%
           addProviderTiles(providers$CartoDB.Positron)
         # addTiles()
       
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
         testing<- as.data.frame(shapeData)
         ss1 <- merge(subzones, shapeData, by.x="SUBZONE_N",
                      by.y="SUBZONE_N", all.x=TRUE)
         # ss1<- as.data.frame(ss1)
         # ss1[is.na(ss1)] <- 0
         # ss1 <- merge(subzones, ss1, by.x="SUBZONE_N",
         #              by.y="SUBZONE_N", all.x=TRUE)
         ss1 <- spTransform(ss1, CRS("+proj=longlat +datum=WGS84 +no_defs"))
         
         
         pcol.C = brewer.pal(input$choroClass,input$choroColor)
         frame.C <- as.data.frame(ss1[,c(monthName)])
         print(frame.C[,1])
         nclass.C =classIntervals(frame.C[,1], n=input$choroClass, style=input$choroMethod,intervalClosure='right', unique=TRUE)
         colcode.C = findColours(nclass.C, pcol.C)
         colcode.C[is.na(colcode.C)] <- "#C0C0C0"
         # print(colcode.C)
         breaks <- unique(unlist(nclass.C$brks, use.names = FALSE))
         
         # print(nclass.C$brks)
         # print(nclass.C$brk)
         pal_volume<-colorBin(na.color = "#C0C0C0",palette = input$choroColor, domain = breaks, 
                              bins =breaks, pretty = FALSE)
         qpal.C <- colorQuantile(input$choroColor, frame.C[,1], n = 6)
         
         
        
         #clear first before adding new layers
         leafletProxy("mymap") %>%
           clearGroup(group="Choropleth")
         
         popup <-paste(paste0("<h4><b>",ss1$SUBZONE_N, "</b></h4>"), paste0("<b>AVG Energy Consumption: </b>",format(round(frame.C[,1], 2), nsmall = 2), " kWh"), sep="<br/>")
         leafletProxy("mymap") %>%
           
           addPolygons(data = ss1, color = "black", fillColor = colcode.C, weight=1,
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
    output$MS <- renderPlot({
      moran.plot(selected_ec_by_subzone[,c(monthName)], rswm_q, zero.policy = TRUE, spChk=FALSE, labels=as.character(selected_ec_by_subzone$SUBZONE_N), xlab="Average Energy Consumption", ylab="Spatially Lag Average Energy Consumption")
    })
     print("END")
  })
  
  
  preloadData <- function(){
    # read private csv 
    privEC2015 <- read.csv ("data/private/2015_Private.csv") 
    privEC2014 <- read.csv ("data/private/2014_Private.csv")
    privEC2013 <- read.csv ("data/private/2013_Private.csv")
    
    # read public csv 
    pubEC2015 <- read.csv ("data/public/2015_Public.csv") 
    pubEC2014 <- read.csv ("data/public/2014_Public.csv")
    pubEC2013 <- read.csv ("data/public/2013_Public.csv")
    
    coordinates(privEC2015) <- ~X+Y
    coordinates(privEC2014) <- ~X+Y
    coordinates(privEC2013) <- ~X+Y
    
    coordinates(pubEC2015) <- ~X+Y
    coordinates(pubEC2014) <- ~X+Y
    coordinates(pubEC2013) <- ~X+Y
    
    # convert files to same CRS
    proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    proj4string(privEC2015) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    proj4string(privEC2014) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    proj4string(privEC2013) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    
    proj4string(pubEC2015) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    proj4string(pubEC2014) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    proj4string(pubEC2013) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    
    # use spatiaEco to map houses to subzones
    privEC_in_subzone_2015 <- point.in.poly(privEC2015, subzones)
    privEC_in_subzone_2015_df <- as.data.frame(privEC_in_subzone_2015)
    privEC_in_subzone_2014 <- point.in.poly(privEC2014, subzones)
    privEC_in_subzone_2014_df <- as.data.frame(privEC_in_subzone_2014)
    privEC_in_subzone_2013 <- point.in.poly(privEC2013, subzones)
    privEC_in_subzone_2013_df <- as.data.frame(privEC_in_subzone_2013)
    
    pubEC_in_subzone_2015 <- point.in.poly(pubEC2015, subzones)
    pubEC_in_subzone_2015_df <- as.data.frame(pubEC_in_subzone_2015)
    pubEC_in_subzone_2014 <- point.in.poly(pubEC2014, subzones)
    pubEC_in_subzone_2014_df <- as.data.frame(pubEC_in_subzone_2014)
    pubEC_in_subzone_2013 <- point.in.poly(pubEC2013, subzones)
    pubEC_in_subzone_2013_df <- as.data.frame(pubEC_in_subzone_2013)
    aggregated_avg_combined_ec_2015_by_subzone <- aggregateECbyMonthBySubzoneCombined(privEC_in_subzone_2015_df, 
                                        pubEC_in_subzone_2015_df, subzones)
    aggregated_avg_combined_ec_2014_by_subzone <- aggregateECbyMonthBySubzoneCombined(privEC_in_subzone_2014_df, 
                                                                                      pubEC_in_subzone_2014_df, subzones)
    aggregated_avg_combined_ec_2013_by_subzone <- aggregateECbyMonthBySubzoneCombined(privEC_in_subzone_2013_df, pubEC_in_subzone_2013_df, subzones) 
            
    aggregated_avg_priv_ec_2015_by_subzone <- aggregateECbyMonthBySubzone(privEC_in_subzone_2015_df, subzones)
    aggregated_avg_priv_ec_2014_by_subzone <- aggregateECbyMonthBySubzone(privEC_in_subzone_2014_df, subzones)
    aggregated_avg_priv_ec_2013_by_subzone <- aggregateECbyMonthBySubzone(privEC_in_subzone_2013_df, subzones)
    
    aggregated_avg_pub_ec_2015_by_subzone <- aggregateECbyMonthBySubzone(pubEC_in_subzone_2015_df, subzones)
    aggregated_avg_pub_ec_2014_by_subzone <- aggregateECbyMonthBySubzone(pubEC_in_subzone_2014_df, subzones)
    aggregated_avg_pub_ec_2013_by_subzone <- aggregateECbyMonthBySubzone(pubEC_in_subzone_2013_df, subzones)
    
    ecList[["2015_priv"]] <<- aggregated_avg_priv_ec_2015_by_subzone
    ecList[["2014_priv"]] <<- aggregated_avg_priv_ec_2014_by_subzone
    ecList[["2013_priv"]] <<- aggregated_avg_priv_ec_2013_by_subzone
    
    ecList[["2015_pub"]] <<- aggregated_avg_pub_ec_2015_by_subzone
    ecList[["2014_pub"]] <<- aggregated_avg_pub_ec_2014_by_subzone
    ecList[["2013_pub"]] <<- aggregated_avg_pub_ec_2013_by_subzone
    
    ecList[["2015_combined"]] <<- aggregated_avg_combined_ec_2015_by_subzone
    ecList[["2014_combined"]] <<- aggregated_avg_combined_ec_2014_by_subzone
    ecList[["2013_combined"]] <<- aggregated_avg_combined_ec_2013_by_subzone
    
   
  }
  
  dataInput <- reactive({
    inFile <- input$files
    if (is.null(inFile))
      return(NULL)
    
    
    fileName <- inFile$name #stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")
    fileList<<- c(fileList, inFile)
    fileNameList<<- c(fileNameList,fileName)
  })
  
  output$dataSelect <- renderUI({
    dataInput()
    selectInput("dataChoice", "Data Selection:", choices = fileNameList)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    fileName <- input$dataChoice
    # data <- fileList$fileName
    # data <- fileList[c(fileName)]
    print(fileName)
    print(fileList)
    
    for(loop in c(1:length(fileList))){
      if(fileName == fileList[[loop]]){
        path <- fileList[[loop+3]]
      }
    }
    
    # file <- fileList[grepl(fileName, names(fileList$name))]
    #file <- fileList$name[fileName]
    # print(file)
    print(path)
    data <- read.csv(path, header=TRUE, sep=",", quote = "\"")
    print(data)
    data
  }))
  
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
  aggregateECbyMonthBySubzoneCombined<- function(privEC_in_subzone_df, pubEC_in_subzone_df, subzones)
  {
    for (month in c(1,2,3,4,5,6,7,8,9,10,11,12)){
      
      privEC_in_subzone_df[, month+1] <- as.numeric(as.character(privEC_in_subzone_df[, month+1] ))
      privEC_in_subzone_df[is.na(privEC_in_subzone_df)] <- 0
      tempPriv<-subset(privEC_in_subzone_df, privEC_in_subzone_df[ , month+1] > 0) 
      pubEC_in_subzone_df[, month+1] <- as.numeric(as.character(pubEC_in_subzone_df[, month+1] ))
      pubEC_in_subzone_df[is.na(pubEC_in_subzone_df)] <- 0
      tempPub<-subset(pubEC_in_subzone_df, pubEC_in_subzone_df[ , month+1] > 0) 
      
      sum_priv__ec_by_subzone <- aggregate(tempPriv[,month+1], 
                                           by=list(SUBZONE_N=tempPriv$SUBZONE_N), FUN=sum)
      sum_pub_ec_by_subzone <- aggregate(tempPub[,month+1], 
                                           by=list(SUBZONE_N=tempPub$SUBZONE_N), FUN=sum)
      count_privpostcodes_in_subzone <- aggregate(tempPriv[,month+1], by=list(SUBZONE_N=tempPriv$SUBZONE_N), FUN=function(x){NROW(x)})
      count_pubpostcodes_in_subzone <- aggregate(tempPub[,month+1], by=list(SUBZONE_N=tempPub$SUBZONE_N), FUN=function(x){NROW(x)})
      
      
      
      sum <- ddply(merge(sum_priv__ec_by_subzone, sum_pub_ec_by_subzone, all=TRUE), 
            .(SUBZONE_N), summarise, x=sum(x))
      
      count <- ddply(merge(count_privpostcodes_in_subzone, count_pubpostcodes_in_subzone, all=TRUE), 
                   .(SUBZONE_N), summarise, x=sum(x))
      # print(count)
      
      # merge aggregate of energy consumption for Jan to subzone
      subzones <- merge(subzones, sum, by.x="SUBZONE_N", by.y="SUBZONE_N", all.x=TRUE)
      names(subzones)[names(subzones)=="x"] <- "sum"
      
      subzones <- merge(subzones, count, by.x="SUBZONE_N", by.y="SUBZONE_N", all.x=TRUE)
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
    return(subzones)
   
  }
  
  aggregateECbyMonthBySubzone <- function(EC_in_subzone_df, subzones){
    for (month in c(1,2,3,4,5,6,7,8,9,10,11,12)){
      EC_in_subzone_df[, month+1] <- as.numeric(as.character(EC_in_subzone_df[, month+1] ))
      EC_in_subzone_df[is.na(EC_in_subzone_df)] <- 0
      # dont include rows with 0s
      temp<-subset(EC_in_subzone_df, EC_in_subzone_df[ , month+1] > 0) 
      aggregate_ec_by_subzone <- aggregate(temp[,month+1], by=list(SUBZONE_N=temp$SUBZONE_N), FUN=sum)
      count_housings_in_subzone <- aggregate(temp[,month+1], by=list(SUBZONE_N=temp$SUBZONE_N), FUN=function(x){NROW(x)})
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
