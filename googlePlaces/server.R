#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

# Define server logic 
shinyServer(function(input, output) {
  
  places <- function(latlng, radius, type) {
    
    key <- 'AIzaSyDgPocAf-cxrzFDaMHctZYoVpsBxpjezZI'
    url <- glue('https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={key}&location={input$latlng}&radius={input$radius}&type={input$type}')
    
    #Use the url to with a get request -> google api
    list <- GET(url = url)
    #Check if there is an error
    if(list$status_code != 200){
      table_results <- data.frame("API ERROR")
    } else {
      results <- content(list)$results
    }
    #Some places dont have a rating: add a NaN rating
    #In fact you should add default values like this for all the fields you are interested in
    for (i in 1:length(results)){
      if(is.null(results[[i]]$rating)){
        results[[i]]$rating = NaN
      }
    }
    
    #Here are the fields we will put in our table
    names <- sapply(results, FUN = get, x="name")
    ratings <- sapply(results, FUN = get, x="rating")
    address <- sapply(results, FUN = get, x="vicinity")
    
    table_results <- data.frame(name = names, rating = ratings, address = address)
    return(table_results)
  }
  
  output$result_table <- renderTable(places(input$latlng, input$radius, input$type))
  
})
