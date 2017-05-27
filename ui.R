#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Air Quality Data Viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Select Year:"),
      sliderInput("year",
                   "Select Year:",
                   min = 2011,
                   max = 2015,
                   value = c(2015,2015),
                  dragRange=TRUE),
      h3("Select Pollutant"),
      uiOutput("pollutantslist"),
      h3("Measure Details:"),
      uiOutput("aggType"),
      uiOutput("units"),
      h3("Selected Station:"),
      textOutput("stationid")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       leafletOutput("mapPlot"),
       plotOutput("histPlot"),
       plotOutput("boxyearPlot")

    )
  )
))
