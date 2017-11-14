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
library(readr)
library(ggplot2)
library(ggthemes)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Where to Meet"),
  
  #We choose a sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      #1st station
      selectizeInput(inputId = "station1", label="Your departure point - metro station (type to avoid going through the list)", 
                     choices = stations, selected = "Bastille", multiple = FALSE, options = NULL),
      
      #2nd station
      selectizeInput(inputId = "station2", label="Your friends' departure point (type to avoid going through the list)",
                     choices = stations, multiple = FALSE, selected = "Bastille", options = NULL),
      
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
      tableOutput("result_table"),
      leafletOutput("leaflet", width = "100%", height = "200px")
    )
  )
))
