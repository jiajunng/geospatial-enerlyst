
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
options(java.parameters = "-Xmx8000m")
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
library(openxlsx)
# rsconnect::setAccountInfo(name='onthefly',
#                           token='680ABC7B5A35A89CC84375F50746490A',
#                           secret='6CFTy6unuRUq0FwOsys9Ndx225l39gzlF8cqUUZP')
# rsconnect::deployApp()
# deployApp()

dfList<-list()
yearList <- list()
fileList <- list()
fileNameList <- list()
month <- 0
ecList<-list()
subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL")
proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
shinyServer(function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  observe({
    #print(input$monthSelector)
    if (length(ecList)==0) {
      preloadData()
    }
    yearSelected<-input$yearSelector
    print(input$yearSelector)
    if(is.null(input$yearSelector)) {
      yearSelected = yearList[[1]]
    }
    monthName <- switch(input$monthSelector, "1" = "Jan", "2" = "Feb", "3" = "Mar",
                        "4" = "Apr", "5" = "May", "6" = "Jun", "7"="Jul","8"="Aug",
                        "9"="Sep","10"="Oct","11"="Nov","12"="Dec")
    #get selected year's data
    # yearSelected <- input$yearSelector
    # unlist(input$yearSelector, "[-]")
    if(input$dataType == "Private")
    {
      yearSelected <- paste(paste(yearSelected,"_"), "priv")
    }else if(input$dataType == "Public")
    {
      yearSelected <- paste(paste(yearSelected,"_"), "pub")
    }
    else
    {
      yearSelected <- paste(paste(yearSelected,"_"), "combined")
    }
    yearSelected <- gsub(" ", "", yearSelected, fixed = TRUE)
    
    #Output Title
    output$title <- renderUI({
      h4(paste(paste("Energy Consumption in ", monthName), paste(" for year ", input$yearSelector)))
    })
    
    selected_ec_by_subzone<-ecList[[yearSelected]] 
    
    # dont include rows with 0s
    selected_ec_by_subzone<-subset(selected_ec_by_subzone, selected_ec_by_subzone[ , monthName] > 0) 
    
    subzones_noNA <- merge(subzones, selected_ec_by_subzone, by.x="SUBZONE_N",
                           by.y="SUBZONE_N", all.x=FALSE)
   
    wm_q <- poly2nb(subzones_noNA, queen=TRUE)
    # print(card(wm_q))
    rswm_q <- nb2listw(wm_q, zero.policy = TRUE)
    
    selected_ec_by_subzone[is.na(selected_ec_by_subzone)] <- 0
    # localMI <- localmoran(aggregated_avg_priv_ec_by_subzone_noNA$monthNames, rswm_q)
    localMI <- localmoran(selected_ec_by_subzone[,c(monthName)], rswm_q)
    
    # print(localMI)
    # summary(localMI)
    # print(rswm_q, zero.policy=TRUE)
    # summary(rswm_q, zero.policy=TRUE)
    # print(subzones_noNA)
    # summary(subzones_noNA)
    localMI[is.na(localMI)] <- 0 #replace Na with 0
    # localMI <- na.omit(localMI) #straight out remove rows with NA
    print(length(localMI[,1]))
    quadrant <- vector(mode="numeric",length=nrow(localMI))
    DV <- selected_ec_by_subzone[,c(monthName)] - mean(selected_ec_by_subzone[,c(monthName)])
    print(DV)
    C_mI <- localMI[,1] - mean(localMI[,1])
    print(C_mI)
    signif <- 0.1
    quadrant[DV >0 & C_mI>0] <- 4
    quadrant[DV <0 & C_mI<0] <- 1
    quadrant[DV <0 & C_mI>0] <- 2
    quadrant[DV >0 & C_mI<0] <- 3
    quadrant[localMI[,5]>signif] <- 0
    print(quadrant)
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
         completeShape <- merge(subzones, shapeData, by.x="SUBZONE_N",
                      by.y="SUBZONE_N", all.x=TRUE)
         completeShape <- spTransform(completeShape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
         frame.C <- as.data.frame(completeShape[,c(monthName)])
         
         pcol.C = brewer.pal(input$choroClass,input$choroColor)
         
         # print(frame.C[,1])
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
         
         popup <-paste(paste0("<h4><b>",completeShape$SUBZONE_N, "</b></h4>"), paste0("<b>AVG Energy Consumption: </b>",format(round(frame.C[,1], 2), nsmall = 2), " kWh"), sep="<br/>")
         leafletProxy("mymap") %>%
           
           addPolygons(data = completeShape, color = "black", fillColor = colcode.C, weight=1,
                       fillOpacity=0.7, group="Choropleth", popup = popup,
                       highlightOptions = highlightOptions(color = "white", weight = 3,bringToFront = TRUE)) %>%
           addLegend(pal = pal_volume, values = frame.C[,1],
                     opacity = 0.7, title="Avg Energy Consumption")
         
       }
       else if (grepl("LISA", input$mymap_groups[1])) {
         # slotNames(shapeData@polygons[[1]])
         # print(slotNames(shapeData@polygons[[1]]))
         # print(shapeData)
         # shapeData <- shapeData[ order(row.names(shapeData)), ]
         # print(shapeData)
         # print(monthName)
         
         # print(shapeData)
         # summary(shapeData)
         # print(shapeData@data)
        
      
         hideChoroControls()
         frame.C <- as.data.frame(shapeData[,c(monthName)])
         frame.C<-frame.C[order(as.numeric(rownames(frame.C))),,drop=FALSE]
         popup <-paste(paste0("<h4><b>",shapeData$SUBZONE_N, "</b></h4>"), 
                       paste0("<b>AVG Energy Consumption: </b>",
                              format(round(frame.C[,1], 2), nsmall = 2), " kWh"), 
                       sep="<br/>")
         # print(colors[findInterval(quadrant,brks,all.inside=FALSE)])
         
         proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
         subzones <- spTransform(subzones, CRS("+proj=longlat +datum=WGS84 +no_defs"))
         # legendColors<-c("#ffffff","#0571b0","#92c5de","#f4a582","red", "d3d3d3")
         # print(legendColors)
         # test<-c(frame.C[,1], c="NA")
         # leafletProxy("mymap", data = subzones) %>%
         # addPolygons(color = "black",
         #             fillColor = "#d3d3d3",
         #             fillOpacity=0.8, weight=1)
         
         leafletProxy("mymap", data = shapeData) %>%
           addPolygons(color = "black",
                       fillColor = colors[findInterval(quadrant,brks,all.inside=FALSE)],
                       fillOpacity=0.8, weight=1, group="LISA",popup = popup) %>%
           addLegend(labels=c("Insignificant","Low-Low","Low-High","High-Low","High-High"),
                     colors=colors,
                     values = frame.C[,1], opacity = 0.7, title="LISA")
       }
       else if (grepl("Local Moran I", input$mymap_groups[1])) {
         hideChoroControls()
         frame.C <- as.data.frame(shapeData[,c(monthName)])
         frame.C<-frame.C[order(as.numeric(rownames(frame.C))),,drop=FALSE]
         popup <-paste(paste0("<h4><b>",shapeData$SUBZONE_N, "</b></h4>"), 
                       paste0("<b>AVG Energy Consumption: </b>",
                              format(round(frame.C[,1], 2), nsmall = 2), " kWh"), 
                       sep="<br/>")
         leafletProxy("mymap", data = shapeData) %>%
           addPolygons(color = "black", fillColor = colcode.LMI ,fillOpacity=0.8,
                       weight = 1, group = "Local Moran I", popup = popup)%>%
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
    privEC2013 <- read.csv ("data/private/2013_Private.csv")
    # read public csv 
    pubEC2013 <- read.csv ("data/public/2013_Public.csv")
    coordinates(privEC2013) <- ~X+Y
    coordinates(pubEC2013) <- ~X+Y
    # convert files to same CRS
    proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    proj4string(privEC2013) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    
    proj4string(pubEC2013) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    privEC_in_subzone_2013 <- point.in.poly(privEC2013, subzones)
    privEC_in_subzone_2013_df <- as.data.frame(privEC_in_subzone_2013)
    
    pubEC_in_subzone_2013 <- point.in.poly(pubEC2013, subzones)
    pubEC_in_subzone_2013_df <- as.data.frame(pubEC_in_subzone_2013)
    aggregated_avg_combined_ec_2013_by_subzone <- aggregateECbyMonthBySubzoneCombined(privEC_in_subzone_2013_df, 
                                                                                      pubEC_in_subzone_2013_df, subzones)
    aggregated_avg_priv_ec_2013_by_subzone <- aggregateECbyMonthBySubzone(privEC_in_subzone_2013_df, subzones)
    
    aggregated_avg_pub_ec_2013_by_subzone <- aggregateECbyMonthBySubzone(pubEC_in_subzone_2013_df, subzones)
    
    ecList[["2013_priv"]]<<- aggregated_avg_priv_ec_2013_by_subzone
    
    ecList[["2013_pub"]] <<- aggregated_avg_pub_ec_2013_by_subzone
    
    ecList[["2013_combined"]]<<- aggregated_avg_combined_ec_2013_by_subzone
    
    yearList<<- c(yearList, "2013")
  }
  
  dataInput <- reactive({
    inFile <- input$newfile
    if (is.null(inFile))
      return(NULL)
    
    fileNameNoExt <- stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)")
    fileYear <- substr(fileNameNoExt, 1, 4)
    yearList <<- c(yearList, fileYear)
    
    fileName <- inFile$name
    fileNameList<<- c(fileNameList, fileNameNoExt)
    
    if (grepl("pub", fileNameNoExt)) {
      year_data<-read.xlsx(inFile$datapath,
                           sheet = 1,
                           colNames=FALSE,
                           startRow=4)
      year_data<-subset(year_data, select=c(X1))
      names(year_data)[names(year_data)=="X1"] <- "Postal"
      
      for(loop in c(1:12)){
        raw_month_data <- read.xlsx(inFile$datapath, 
                                    sheet = loop,
                                    colNames=FALSE,
                                    startRow=4)
        
        raw_month_data[2:5] <- lapply( raw_month_data[2:5], function(col) as.numeric( gsub("-$|\\,", "", col) ) )
        raw_month_data[is.na(raw_month_data)] <- 0
        
        raw_month_data[,2] <- as.numeric(as.character(raw_month_data[,2] ))
        raw_month_data[,3] <- as.numeric(as.character(raw_month_data[,3] ))
        raw_month_data[,4] <- as.numeric(as.character(raw_month_data[,4] ))
        raw_month_data[,5] <- as.numeric(as.character(raw_month_data[,5] ))
        raw_month_data[,8] <- rowSums(raw_month_data[, c(2, 3, 4, 5)])
        
        year_data <- merge(x = year_data, y = raw_month_data, by.x='Postal', by.y='X1', all.x = TRUE)
        
        monthName <- switch(loop, "1" = "Jan", "2" = "Feb", "3" = "Mar",
                            "4" = "Apr", "5" = "May", "6" = "Jun", "7"="Jul","8"="Aug",
                            "9"="Sep","10"="Oct","11"="Nov","12"="Dec")
        names(year_data)[names(year_data)=="V8"] <- monthName
        
        year_data<-subset(year_data, select=-c(X5))
        year_data<-subset(year_data, select=-c(X4))
        year_data<-subset(year_data, select=-c(X3))
        year_data<-subset(year_data, select=-c(X2))
        
        
        if (loop != 12) {
          year_data<-subset(year_data, select=-c(X7))
          year_data<-subset(year_data, select=-c(X6))
        } else {
          names(year_data)[names(year_data)=="X6"] <- "X"
          names(year_data)[names(year_data)=="X7"] <- "Y"
        }
      }
      
      # reorder columns
      year_data <- year_data[c(1,2,3,4,5,6,7,8,9,10,11,12,15,13,14)]
      
      fileList[[fileNameNoExt]] <<- year_data
      
    } else if (grepl("priv", fileNameNoExt)) {
      year_data<-read.xlsx(inFile$datapath,
                           sheet = 1,
                           colNames=TRUE,
                           startRow=3)
      
      names(year_data)[names(year_data)=="Postal.Code"] <- "Postal"
      names(year_data)[names(year_data)=="X"] <- "X"
      names(year_data)[names(year_data)=="Y"] <- "Y"
      
      fileList[[fileNameNoExt]] <<- year_data
    }
    
    # clean up na and s
    year_data$Jan <- as.character(year_data$Jan)
    year_data$Jan[year_data$Jan == "s"] <- "0"
    year_data$Feb <- as.character(year_data$Feb)
    year_data$Feb[year_data$Feb == "s"] <- "0"
    year_data$Mar <- as.character(year_data$Mar)
    year_data$Mar[year_data$Mar == "s"] <- "0"
    year_data$Apr <- as.character(year_data$Apr)
    year_data$Apr[year_data$Apr == "s"] <- "0"
    year_data$May <- as.character(year_data$May)
    year_data$May[year_data$May == "s"] <- "0"
    year_data$Jun <- as.character(year_data$Jun)
    year_data$Jun[year_data$Jun == "s"] <- "0"
    year_data$Jul <- as.character(year_data$Jul)
    year_data$Jul[year_data$Jul == "s"] <- "0"
    year_data$Aug <- as.character(year_data$Aug)
    year_data$Aug[year_data$Aug == "s"] <- "0"
    year_data$Sep <- as.character(year_data$Sep)
    year_data$Sep[year_data$Sep == "s"] <- "0"
    year_data$Oct <- as.character(year_data$Oct)
    year_data$Oct[year_data$Oct == "s"] <- "0"
    year_data$Nov <- as.character(year_data$Nov)
    year_data$Nov[year_data$Nov == "s"] <- "0"
    year_data$Dec <- as.character(year_data$Dec)
    year_data$Dec[year_data$Dec == "s"] <- "0"
    year_data[is.na(year_data)] <- 0
    
    year_data[, 1] <- as.integer(as.character(year_data[, 1]))
    year_data$X <- as.numeric(as.character(year_data$X)) 
    year_data$Y <- as.numeric(as.character(year_data$Y)) 
    # shang mian is int + factor. x-y are num
    
    
    # year_data <- na.omit(year_data)
    coordinates(year_data) <- ~X+Y
    subzones <- readOGR(dsn = "data/shp", layer = "MP14_SUBZONE_NO_SEA_PL")
    proj4string(subzones) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    proj4string(year_data) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")
    
    year_data_in_subzone <- point.in.poly(year_data, subzones)
    year_data_in_subzone_df <- as.data.frame(year_data_in_subzone)
    dfList[[fileNameNoExt]] <<- year_data_in_subzone_df
    aggregated_avg_ec_by_subzone <- aggregateECbyMonthBySubzone(year_data_in_subzone_df, subzones)
    ecList[[fileNameNoExt]] <<- aggregated_avg_ec_by_subzone
    
    
    print("combining...")
    priv_file_name <- paste(fileYear, "_priv", sep = "")
    pub_file_name <- paste(fileYear, "_pub", sep = "")
    
    print(priv_file_name)
    print(pub_file_name)
    
    if (!is.null(ecList[[priv_file_name]])) {
      if(!is.null(ecList[[pub_file_name]])) {
        priv_df <- dfList[[priv_file_name]]
        pub_df <- dfList[[pub_file_name]]
        combined <- aggregateECbyMonthBySubzoneCombined(priv_df, pub_df, subzones)
        print(combined)
        combined_name <- paste(fileYear, "_combined", sep = "")
        ecList[[combined_name]] <<- combined
      }
    }
    print("upload ok")
  })
  
  output$dataSelect <- renderUI({
    dataInput()
    selectInput("dataChoice", "View uploaded data:", choices = fileNameList)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    fileName <- input$dataChoice
    data <- ecList[[fileName]]
    return(data)
  }))
  
  output$dataSelect <- renderUI({
    dataInput()
    selectInput("dataChoice", "Data Selection:", choices = fileNameList)
  })
  
  
  output$yearSelect <- renderUI({
    dataInput()
    print(yearList)
    selectInput("yearSelector", "Select a Year:", choices = yearList)
  })
  
  
  observeEvent(input$togglePlot, {
    toggle("MS")
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
