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
library(shinythemes)
df_total_final <- readRDS("final_data_331.rds")
df_names_final <- readRDS("final_names_331.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  #Theme
  theme = shinythemes::shinytheme("sandstone"),
  
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
      DTOutput("investmentTable"),
      uiOutput("text_box")
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


  output$text_box <- renderUI({
      text <- "The Efficient Market Hypothesis (EMH) is a fundamental concept in finance that suggests financial markets are efficient in reflecting all available information. Put simply, it implies that asset prices already incorporate all known information and that it's impossible to consistently beat the market by outsmarting it.<br><br>

The relevance of the Efficient Market Hypothesis to someone getting started in investing is significant. Here's how:<br><br>

&emsp;1. Investment Strategies: EMH suggests that it's difficult to consistently beat the market through stock picking or market timing. Just think about it… there are entire companies dedicated to doing what you as a single investor are trying to do. Instead of trying to outsmart other investors, focus on learning the fundamentals of investing, the importance of staying disciplined in your investment approach, and adopt passive investment strategies, such as investing in index funds or exchange-traded funds (ETFs), which aim to match the performance of the overall market rather than trying to beat it.<br><br>
 
&emsp;2. Risk Management: EMH underscores the importance of diversification as a risk management strategy. Since it's difficult to predict which individual stocks will outperform the market, spreading investments across a diversified portfolio can help mitigate risk.<br><br>

&emsp;3. Long-Term Perspective: EMH suggests that short-term fluctuations in asset prices are largely unpredictable. Therefore, you should focus on long-term investing goals and avoid making impulsive decisions based on short-term market movements.<br><br>

We encourage you to explore the EHM in real time above! You will see the returns over five years if you were to pick the five best performing stocks from your selected year. This reflects a simple strategy you might select to try to beat the market: just pick the best performing stocks. You will see that some years you will win (beat the market), but others you will not. It is important to take even the wins with a grain of salt as the best performing stocks exhibit momentum.<br><br>

Momentum in stock performance refers to the tendency of stocks that have performed well in the past to continue performing well in the near future, and stocks that have performed poorly to continue performing poorly. This concept is based on the idea that investors tend to follow trends and exhibit herd behavior, causing prices to continue moving in the same direction for a period of time.<br><br>

The takeaway is this: over long-periods of time this momentum will slow and you will start losing to the market. For investors of all levels, but especially beginners, it is a good idea to keep the EHM front of mind and invest in index funds like the S&P 500.
"
      
      HTML(paste0("<p>", text, "</p>"))
    })
  
}  


# Run the application 
shinyApp(ui = ui, server = server)





