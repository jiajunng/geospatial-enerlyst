
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(leaflet)
library(shiny)
library(shinyjs)





shinyUI(fluidPage(
  tags$head(tags$style(
    HTML('
        #MS {
        background-color: #ffffff;
          padding: 15px;            
        opacity: 0.9;
        }
         #sidebar {
            background-color: rgba(255,255,255,0.7);
            padding: 15px;
         }
        

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  # Application title
  tabsetPanel(
   
    tabPanel("A very nice Map",
             leafletOutput("mymap", height = "800px"),
             
             absolutePanel(id="sidebar",top = 100, left = 50,
                           uiOutput("title"),
                           
                           
                           fileInput('inputdata', 'Input shapefile',
                                     accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                                     multiple=TRUE),
                           #checkboxInput("legend", "Show legend", TRUE),
                              
                           uiOutput("choose"),
                           uiOutput("year"),
                           sliderInput("monthSelector", "Select a Month:", 
                                       min = 1, max = 12, value = 1, step= 1),
                           
                           absolutePanel(top=67, left=360, width=465, draggable=TRUE, 
                                         useShinyjs(),
                                         actionButton("togglePlot", "Show/Hide plot"),
                                         plotOutput("MS")
                           )
                           
                           
             ))
  )
))
