
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)

# ui<- fluidPage()
# server <- function(input, output){}
# shinyApp(ui=ui, server=server)
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # selectInput("dataset1", "Choose a dataset:",
      #             choices = c("shape", "pressure", "cars")),
      fileInput('inputdata', 'Input shapefile',
                accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                multiple=TRUE),
      # fileInput('layer', 'Choose shp File as Layer',
      #           accept=c('text/csv', 
      #                    'text/comma-separated-values,text/plain', 
      #                    '.csv')),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      uiOutput("choose")
      # checkboxGroupInput("checkGroup", label = h3("Checkbox group"),
      #                    choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
      #                    selected = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      # uiOutput("text")
      plotOutput("value")
      
    )
  )
))
