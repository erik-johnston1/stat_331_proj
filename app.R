#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse)
source("df_proj.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investing"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Year",
                  "Select Year:",
                  min = 2009,
                  max = 2018,
                  value = 2014)
                  
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("Plot")
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot <- renderPlot({
        # assign the dataframe relating to the year as a new dataframe called df
      year <- as.character(input$Year)

      df <- get (paste0("df_", year, "_final"))
    
      
        # draw the ggplot

       df %>%
          ggplot(aes(x = date)) +
          geom_line(aes(y = SP500_Cum * 5000, color = "S&P 500"), linetype = "solid") +
          geom_line(aes(y = RAND_Cum * 5000, color = "Created Portfolio"), linetype = "solid") +
          labs(title = "Portfolio Growth Comparison",
               subtitle = "Investment Value",
               y = NULL,
               x = "Years After Start Date",
               color = "Legend") +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
          theme_bw()
         

            })
    }

# Run the application 
shinyApp(ui = ui, server = server)
