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
library(igraph)
library(forcats)

# Define server logic 
shinyServer(function(input, output) {
  
  temps_trajet <- read.csv2('../temps_trajet.csv')
  base_trajet_total <- read.csv2('../base_trajet_total.csv')
  
  # table de correspondance nom de stations/trip_id
  nodes_trajet <- base_trajet_total[, 1:2]
  nodes_trajet <- unique(nodes_trajet)
  
  temps_trajet_igraph <- graph_from_data_frame(d=temps_trajet, vertices = nodes_trajet, directed = T)
  
  
  #Function which finds the optimal solution (where to meet)
  
  best_path <- function(station1, station2){
    
    if(station1 == station2){
      return("You don't need this app, you live in the same place !")
    }
    
    else{
      
      #We will put the solution in a list
      sol <- list()
      
      trajet_plus_court <- shortest_paths(temps_trajet_igraph, V(temps_trajet_igraph)[stop_name == station1], to = V(temps_trajet_igraph)[stop_name == station2], output = "vpath")
      
      
      #Ici vous avez le temps total de trajet : il faut encore l'input de l'utilisateur
      
      df_distances <- as.data.frame(distances(temps_trajet_igraph, V(temps_trajet_igraph)[stop_name == station1], to = V(temps_trajet_igraph)[stop_name == station2]))
      
      
      inds = which(df_distances == min(df_distances), arr.ind=TRUE)
      n_colonne <- inds[1, 2] # IMPORTANT pour choisir les stations par la suite
      
      sol$path$stop_ids <- V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name
      
      ###ON A OBTENU LE CHEMIN IDEAL, MAINTENANT IL FAUT TROUVER LE MIDPOINT
      
      # pathlist_exclu comprend tous les éléments de l'itinéraire sauf le dernier (1ère colonne de chemin)
      
      pathlist_exclu <-c()
      for (i in 1:(length(V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name)-1)){
        pathlist_exclu[i] <- trajet_plus_court$vpath[[n_colonne]]$name[i]
      }
      
      #database chemin avec uniquement les trajets de l'itinéraire, dans le bon sens
      
      chemin <- temps_trajet %>% filter(from_stop_id %in% V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name, to_stop_id %in% V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name) %>% slice(match(pathlist_exclu, from_stop_id))
      
      
      # cumsum des temps de trajet
      
      chemin <- chemin %>% transform(Total = ave(weight, FUN = cumsum))
      
      
      #mi-chemin:
      
      mid <- max(chemin$Total)/2
      
      sol$mid$stop_id <- (chemin %>% filter( Total > mid ))$to_stop_id[1]
      sol$mid$stop_name <- (nodes_trajet %>% filter(stop_id == sol$mid$stop_id))[2]
      sol$mid$lat_lng <- paste((base_trajet_total %>% filter(stop_id == sol$mid$stop_id))[1,4], ',',
                               (base_trajet_total %>% filter(stop_id == sol$mid$stop_id))[1,5], sep = '')
      
      ########
      #Now we prepare our graph
      base_trajet_court <- base_trajet_total %>% 
        filter (stop_id %in% V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name )
      
      base_metro_map <- base_trajet_total %>%
        distinct(stop_name, route_short_name, .keep_all = TRUE)
      
      base_metro_map$route_short_name = factor(base_metro_map$route_short_name)
      
      base_trajet_total$route_short_name = factor(base_trajet_total$route_short_name)
      
      factpal=colorFactor(palette = c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"), 
                          domain = base_metro_map$route_short_name, levels = levels(base_metro_map$route_short_name), ordered = TRUE, na.color = "#808080", alpha = FALSE)
      
      map_metro_ligne = leaflet() %>%
        # Add CartoDB background map
        addProviderTiles("CartoDB.DarkMatter") %>%  
        # Add a marker for each stop
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 1, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FFCD00")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 2, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#003CA6")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 3, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#837902")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "3B", direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#00AE41")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 4, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#CF009E")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 5, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FF7E2E")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 6, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 7, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 7, trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "7B", trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "7B", trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 8, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E19BDF")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 9, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#B6BD00")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 10), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#C9910D")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 11, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#704B1C")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 12, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#007852")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 13, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 13, trip_short_name ==102), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 14, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#62259D")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "A", trip_headsign == "NELY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "A", trip_headsign == "QIKY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "B", trip_headsign == "SOIR"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
        addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "B", trip_headsign == "KOCQ"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
        
        addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court, stroke = FALSE, 
                         fillOpacity = 0.5, radius =4, color = ~ factpal(route_short_name)) %>% 
        addLegend(colors =c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"),
                  labels = levels(base_trajet_total$route_short_name),title = "Metro line Paris")
      
      sol$map <- map_metro_ligne
      
      return(sol)
    }
  }
  
  ########################
  #Google maps part
  
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
  
  ############################Output part
  
  solution <- reactive({best_path(input$station1, input$station2)})
  output$solution <- renderPrint(print(solution()$mid))
  output$leaflet <- renderLeaflet(solution()$map)
  output$result_table <- renderTable(places(solution()$mid$lat_lng, input$radius, input$type))
})


#######################################
##Use this if you want the full paris metro map

# #Prepare the color palette
# factpal=colorFactor(palette = c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"), 
#                     domain = base_trajet_total$route_short_name, levels = levels(base_trajet_total$route_short_name), ordered = TRUE, na.color = "#808080", alpha = FALSE)
# #Code the plot
# myplot = leaflet() %>%
#   # Add CartoDB background map
#   addProviderTiles("CartoDB.DarkMatter") %>%  
#   # Add a marker for each stop
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 1, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FFCD00")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 2, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#003CA6")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 3, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#837902")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "3B", direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#00AE41")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 4, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#CF009E")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 5, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FF7E2E")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 6, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 7, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 7, trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "7B", trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "7B", trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 8, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E19BDF")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 9, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#B6BD00")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 10), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#C9910D")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 11, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#704B1C")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 12, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#007852")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 13, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 13, trip_short_name ==102), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 14, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#62259D")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "A", trip_headsign == "NELY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "A", trip_headsign == "QIKY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "B", trip_headsign == "SOIR"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "B", trip_headsign == "KOCQ"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
#   
#   addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total, stroke = FALSE, 
#                    fillOpacity = 0.5, radius =4, color = ~ factpal(route_short_name)) %>% 
#   addLegend(colors =c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"),
#             labels = levels(base_trajet_total$route_short_name)[1:18],title = "Metro line Paris")
# 
