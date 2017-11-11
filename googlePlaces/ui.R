#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(glue)
library(jsonlite)
library(dplyr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Where to Meet"),
  
  #We choose a sidebar layout
  sidebarLayout(
    sidebarPanel(
      #Text input
      textInput(inputId = "latlng", 
                label = "Meeting point lattitude and longitude:", 
                value = "40.714224,-73.961452"),
      #Slider
      sliderInput("radius",
                  "Radius around meeting point in metres:",
                  min = 10,
                  max = 500,
                  value = 200),
      #Selecter input
      selectInput(inputId = "type",
                  label = "Type of results: ",
                  selected = 3, 
                  choices = c('cafe', 'bar', 'restaurant'))
    ),
    
    mainPanel(
      tableOutput("result_table")
    )
  )
))
