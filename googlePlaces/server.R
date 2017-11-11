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
library(readr)
library(ggplot2)
library(ggthemes)
library(leaflet)

# Define server logic 
shinyServer(function(input, output) {

  
  base_trajet_total <- read.csv2("../../data/base_trajet_total.csv")
  stations = unique(base_trajet_total$stop_name)
  
  #Function which finds the optimal station with the algorithm
  #Since we dont have the algorith, set it to a default
  result_station <- function(station1, station2){
    res <- "40.714224,-73.961452"
    return(res)
  } 
  
  places <- function(latlng, radius, type) {
    
    key <- 'AIzaSyDgPocAf-cxrzFDaMHctZYoVpsBxpjezZI'
    url <- glue('https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={key}&location={latlng}&radius={input$radius}&type={input$type}')
    
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
  
  #######################
  #Plot part
  
  #Prepare the color palette
  factpal=colorFactor(palette = c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"), 
                      domain = base_trajet_total$route_short_name, levels = levels(base_trajet_total$route_short_name), ordered = TRUE, na.color = "#808080", alpha = FALSE)
  #Code the plot
  myplot = leaflet() %>%
    # Add CartoDB background map
    addProviderTiles("CartoDB.DarkMatter") %>%  
    # Add a marker for each stop
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 1, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FFCD00")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 2, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#003CA6")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 3, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#837902")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "3B", direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#00AE41")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 4, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#CF009E")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 5, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FF7E2E")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 6, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 7, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 7, trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "7B", trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "7B", trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 8, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E19BDF")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 9, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#B6BD00")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 10), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#C9910D")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 11, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#704B1C")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 12, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#007852")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 13, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 13, trip_short_name ==102), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 14, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#62259D")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "A", trip_headsign == "NELY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "A", trip_headsign == "QIKY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "B", trip_headsign == "SOIR"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
    addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "B", trip_headsign == "KOCQ"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
    
    addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total, stroke = FALSE, 
                     fillOpacity = 0.5, radius =4, color = ~ factpal(route_short_name)) %>% 
    addLegend(colors =c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"),
              labels = levels(base_trajet_total$route_short_name)[1:18],title = "Metro line Paris")
  
  ############################Output part 
  latlng = result_station(input$station1, input$station2)
  output$result_table <- renderTable(places(latlng, input$radius, input$type))
  output$leaflet <- renderLeaflet(myplot)
})
