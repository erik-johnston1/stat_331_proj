#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(bslib)
library(shiny)
library(lubridate)
library(tidyverse)
library(DT)
df_total_final <- readRDS("final_data_331.rds")
df_names_final <- readRDS("final_names_331.rds")

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
                  value = 2014,
                  sep = ""),
      sliderInput("investmentAmount",
                  "Select Investment Amount:",
                  min = 1000,
                  max = 5000,
                  value = 3000, # Default value
                  step = 250),
      
      DTOutput("StocksTable")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot", width = "100%", height = "500px"),
      DTOutput("investmentTable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    
    df <- df_total_final %>% 
      filter(year_inception == as.character(input$Year))
    
    # draw the ggplot
    
    df %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = SP500_Cum * input$investmentAmount, color = "S&P 500"), linetype = "solid") +
      geom_line(aes(y = RAND_Cum * input$investmentAmount, color = "Top Preforming Stocks"), linetype = "solid") +
      labs(title = "Portfolio Growth Comparison",
           subtitle = "Investment Value",
           y = NULL,
           x = "Next Five Years After Investing",
           color = "Legend") +
      scale_y_continuous(limits = c(0, 35000), breaks = scales::pretty_breaks(n = 5)) +
      theme_bw()
    
  })
  
  output$investmentTable <- renderDT({
    
    df <- df_total_final %>% 
      filter(year_inception == as.character(input$Year))
    
    lastRow <- tail(df, 1)
    
    dataTable <- data.frame(
      "Investment" = c("S&P 500", "Portfolio of Top Performing Stocks This Year"),
      "Final Investment Value After Five Years" = c(round(lastRow$SP500_Cum[1] * input$investmentAmount, 2), round(lastRow$RAND_Cum[1] * input$investmentAmount, 2)),
    check.names = FALSE)
    
    datatable(dataTable, options = list(searching = FALSE, lengthChange = FALSE, paging = FALSE, info = FALSE),
              rownames = FALSE)
    
    
  })
  

  output$StocksTable <- renderDT({
    
    df_names <- df_names_final %>% 
      filter(year == as.character(input$Year))
    
    # Convert the vector of characters to a single string
    
    dataTable <- data.frame(
    "Ticker" = c(df_names$ticker),
    "Company Name" = c(df_names$name),
    check.names = FALSE
    )
    datatable(dataTable, options = list(searching = FALSE, lengthChange = FALSE, paging = FALSE, info = FALSE),
              rownames = FALSE, # Remove row names if not needed
              caption = htmltools::tags$caption(htmltools::tags$h3("Best Performing Stocks This Year")))
})
  
}  


# Run the application 
shinyApp(ui = ui, server = server)





