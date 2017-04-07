
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
             
             absolutePanel(id="sidebar",top = 100, left = 50, width = 320,
                           uiOutput("title"),
                           
                           
                           # fileInput('inputdata', 'Input shapefile',
                           #           accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                           #           multiple=TRUE),
                           #checkboxInput("legend", "Show legend", TRUE),
                              
                           uiOutput("choose"),
                           uiOutput("year"),
                           sliderInput("monthSelector", "Select a Month:", 
                                       min = 1, max = 12, value = 1, step= 1),
                           
                          
                             fluidRow(
                               column(4, numericInput("choroClass", "Classes:", 4, min = 2, max = 100)),
                               column(8,selectInput("choroColor", "Color:",
                                                    c("Blues"= "Blues","BrBG" = "BrBG","PiYG " = "PiYG","PRGn " = "PRGn",
                                                      "PuOr " = "PuOr","RdBu " = "RdBu","RdGy " = "RdGy",
                                                      "RdYlBu" = "RdYlBu","RdYlGn" = "RdYlGn","Spectral" = "Spectral" )))
                             ),
                             fluidRow(
                               column(12,selectInput("choroMethod", "Classification Method:",
                                                     c("Jenks Natural Breaks" = "jenks",
                                                       "Equal Interval" = "equal",
                                                       "Quantile" = "quantile")))
                             )
                             
                           
                           
                           ,
                           absolutePanel(top=500, left=50, width=465, 
                                         draggable=TRUE, fixed=TRUE,
                                         useShinyjs(),
                                         actionButton("togglePlot", "Show/Hide Scatterplot"),
                                         plotOutput("MS"))
                           
                           
             ))
  )
))
