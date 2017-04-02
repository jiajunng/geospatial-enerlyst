
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(leaflet)
library(shiny)

shinyUI(fluidPage(

  # Application title
  tabsetPanel(
   
    tabPanel("A very nice Map",
             leafletOutput("mymap", height = "800px"),
             absolutePanel(top=100, left= 50, h4("R, YOU ALRIGHT?")),
             absolutePanel(top = 200, left = 50,
                           fileInput('inputdata', 'Input shapefile',
                                     accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                                     multiple=TRUE),
                           checkboxInput("legend", "Show legend", TRUE),
                              
                           uiOutput("choose"),
                           selectInput("dataset", "Choose a dataset:",
                                       choices = c("rock", "pressure", "cars")),
                           sliderInput("monthSelector", "Integer:", 
                                       min = 0, max = 12, value = 1, step= 1)
                           
                           
             ))
  )
))
