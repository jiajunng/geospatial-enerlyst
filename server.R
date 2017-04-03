
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

shpListName <-list()
shpList<-list()

shinyServer(function(input, output) {
  in_data <- reactive({
    shpListName<<-c(shpListName,gsub("\\..*","",input$inputdata$name[1]))
  })
  
  observe({
    print(input$monthSelector)
    
    subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL") # read shapefile
    
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
    #print(aggregated_avg_priv_ec_by_subzone$X1)
    #print(na.omit(aggregated_avg_priv_ec_by_subzone$X1))
    aggregated_avg_priv_ec_by_subzone_noNA <- completeFun(aggregated_avg_priv_ec_by_subzone, "X1")
    subzones_noNA <- merge(subzones, aggregated_avg_priv_ec_by_subzone_noNA, by.x="SUBZONE_N", by.y="SUBZONE_N", all.x=FALSE)
    
    wm_q <- poly2nb(subzones_noNA, queen=TRUE)
    rswm_q <- nb2listw(wm_q, zero.policy = TRUE)
    
    aggregated_avg_priv_ec_by_subzone[is.na(aggregated_avg_priv_ec_by_subzone)] <- 0
    localMI <- localmoran(aggregated_avg_priv_ec_by_subzone_noNA$X1, rswm_q)
    #print(localMI)
    
    #localMI[is.na(localMI)] <- 0 #replace Na with 0
    localMI <- na.omit(localMI) #straight out remove rows with NA
    
    #localMI[complete.cases(localMI),]
    
    print(localMI)
    
    localMI.shade <- auto.shading(c(localMI[,1],-localMI[,1]),cols=brewer.pal(5,"PRGn"))
    choropleth(subzones_noNA,localMI[,1],shading=localMI.shade)
    choro.legend(170000, 2860000,localMI.shade,cex=0.7)
    title("Hunan GDPPC 2012 (local Moran I)", cex.main=1.2)
    
    quadrant <- vector(mode="numeric",length=nrow(localMI))
    print(quadrant)
    DV <- aggregated_avg_priv_ec_by_subzone_noNA$X1 - mean(aggregated_avg_priv_ec_by_subzone_noNA$X1)
    print(DV)
    #print(localMI)
    C_mI <- localMI[,1] - mean(localMI[,1])
    print(C_mI)
    signif <- 0.1
    quadrant[DV >0 & C_mI>0] <- 4
    quadrant[DV <0 & C_mI<0] <- 1
    quadrant[DV <0 & C_mI>0] <- 2
    quadrant[DV >0 & C_mI<0] <- 3
    quadrant[localMI[,5]>signif] <- 0
    
    brks <- c(0,1,2,3,4)
    colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
    plot(subzones_noNA,border="lightgray",
         col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
    box()
    legend("bottomright",legend=c("insignificant","low-low","low-high","high-low","high-high"),
           fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)
    title("LISA Cluster Map")
    proj4string(subzones_noNA) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
     shapeData1 <- spTransform(subzones_noNA, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    leafletProxy("mymap") %>%
      addTiles() %>%
      addPolygons(data = subzones, color = "FFFFFF", fillColor = "red")
    print("test2")
    
    
    # subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL")
    # 
    # proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    # shapeData1 <- spTransform(subzones, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    proxy <- leafletProxy("mymap") %>%
      addPolygons(data = shapeData1, color="black", fillColor="orange", fillOpacity=1)
    
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
    
    # subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL")
    # 
    # proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    # shapeData1 <- spTransform(subzones, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    
    leafletProxy("mymap") %>%
      addMarkers(data = shapeData)
      
  })
  
  # ############################## LATEST ######################################################
  # observe({
  #  
  #   subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL") # read shapefile
  #   
  #   # read private housing energy consumption data
  #   privEC <- read.csv ("data/private/private_housing.csv") # read csv
  #   coordinates(privEC) <- ~X+Y
  #   # convert files to same CRS
  #   proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
  #   proj4string(privEC) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
  #   
  #   # use spatiaEco to map houses to subzones
  #   privEC_in_subzone <- point.in.poly(privEC, subzones)
  #   privEC_in_subzone_df <- as.data.frame(privEC_in_subzone)
  #   
  #   
  #   aggregated_avg_priv_ec_by_subzone <- aggregateECbyMonthBySubzone(privEC_in_subzone_df, subzones)
  #   #print(aggregated_avg_priv_ec_by_subzone$X1)
  #   #print(na.omit(aggregated_avg_priv_ec_by_subzone$X1))
  #   aggregated_avg_priv_ec_by_subzone_noNA <- completeFun(aggregated_avg_priv_ec_by_subzone, "X1")
  #   subzones_noNA <- merge(subzones, aggregated_avg_priv_ec_by_subzone_noNA, by.x="SUBZONE_N", by.y="SUBZONE_N", all.x=FALSE)
  #   
  #   wm_q <- poly2nb(subzones_noNA, queen=TRUE)
  #   rswm_q <- nb2listw(wm_q, zero.policy = TRUE)
  #   
  #   aggregated_avg_priv_ec_by_subzone[is.na(aggregated_avg_priv_ec_by_subzone)] <- 0
  #   localMI <- localmoran(aggregated_avg_priv_ec_by_subzone_noNA$X1, rswm_q)
  #   #print(localMI)
  #   
  #   #localMI[is.na(localMI)] <- 0 #replace Na with 0
  #   localMI <- na.omit(localMI) #straight out remove rows with NA
  #   
  #   #localMI[complete.cases(localMI),]
  #  
  #   print(localMI)
  # 
  #   localMI.shade <- auto.shading(c(localMI[,1],-localMI[,1]),cols=brewer.pal(5,"PRGn"))
  #   choropleth(subzones_noNA,localMI[,1],shading=localMI.shade)
  #   choro.legend(170000, 2860000,localMI.shade,cex=0.7)
  #   title("Hunan GDPPC 2012 (local Moran I)", cex.main=1.2)
  #   
  #   quadrant <- vector(mode="numeric",length=nrow(localMI))
  #   print(quadrant)
  #   DV <- aggregated_avg_priv_ec_by_subzone_noNA$X1 - mean(aggregated_avg_priv_ec_by_subzone_noNA$X1)
  #   print(DV)
  #   #print(localMI)
  #   C_mI <- localMI[,1] - mean(localMI[,1])
  #   print(C_mI)
  #   signif <- 0.1
  #   quadrant[DV >0 & C_mI>0] <- 4
  #   quadrant[DV <0 & C_mI<0] <- 1
  #   quadrant[DV <0 & C_mI>0] <- 2
  #   quadrant[DV >0 & C_mI<0] <- 3
  #   quadrant[localMI[,5]>signif] <- 0
  # 
  #   brks <- c(0,1,2,3,4)
  #   colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
  #   plot(subzones_noNA,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
  #   box()
  #   legend("bottomright",legend=c("insignificant","low-low","low-high","high-low","high-high"),
  #          fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)
  #   title("LISA Cluster Map")
  #   proj4string(subzones_noNA) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
  #  
  #   leafletProxy("mymap") %>%
  #     addTiles() %>%
  #     addPolygons(data = subzones, color = "03F")
  #   print("test2")
  # })
  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  
  aggregateECbyMonthBySubzone <- function(privEC_in_subzone_df, subzones){
    for (month in c(1,2,3,4,5,6,7,8,9,10,11,12)){
      privEC_in_subzone_df[, month+1] <- as.numeric(as.character(privEC_in_subzone_df[, month+1] ))
      privEC_in_subzone_df[is.na(privEC_in_subzone_df)] <- 0
      aggregate_ec_by_subzone <- aggregate(privEC_in_subzone_df[,month+1], by=list(SUBZONE_N=privEC_in_subzone_df$SUBZONE_N), FUN=sum)
      
      # merge aggregate of energy consumption for Jan to subzone
      subzones <- merge(subzones, aggregate_ec_by_subzone, by.x="SUBZONE_N", by.y="SUBZONE_N", all.x=TRUE)
      
      # get energy consumption in Jan per km square of subzone
      subzones <- transform(subzones, avg = x / (SHAPE_Area / 1000000))
      #rename(merged, c("avg"="montesh"))
      names(subzones)[names(subzones)=="avg"] <- month
      
      #drop the x column since incoming iteration would be using the same name
      subzones<-subset(subzones, select=-c(x))
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
  output$value <- renderPlot({ 
    selectedShp=input$selectedShp
    shpListName
    test<-shpList[1]
    test1 <- do.call(rbind, lapply(test, data.frame, stringsAsFactors=FALSE))
    coordinates(test1) <-~XCOORD+YCOORD
    plot(test1)
    })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)%>%
      addTiles()
  })
  
  
})
