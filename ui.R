

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Tube map optimisation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    selectInput("variable", "Departure tubestop/line",
                list("Châtelet" = "cha", 
                     "Saint-Paul" = "saint", 
                     "Louvre" = "louvre",
                     "Concorde" = "conc")),
    checkboxInput("outliers", "Show outliers", FALSE)
  ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
