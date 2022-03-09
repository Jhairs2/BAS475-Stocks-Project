
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(quantmod)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyquant)
library(shinyWidgets)
library(dashboardthemes)

# Configuring settings as per tidyquant tutorial
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)



# Define function to split names on dot and only keep after dot text
clean_names <- function(stocks) {
  split_names = strsplit(names(stocks), split = '.', fixed = TRUE)
  vapply(split_names, function(x) x[2], character(1))
}


# Getting stock symbols and creating date range
stockNames <- stockSymbols()[,1]
start <- as.Date("2010-01-01")
end <- Sys.Date()

ui <- dashboardPage( skin = "yellow", 
                     
                     dashboardHeader( title = "Stock Analysis"),

    
    dashboardSidebar(
        
        # User Will choose stock Here
        selectizeInput("chooseStock", choices = NULL, label = h3("Choose a stock")),
        
        # User can choose date range for stock
        dateRangeInput(
            "chooseDate",
            label = h3("Choose a Date")
        )
        
    ),
    
    dashboardBody(
        shinyDashboardThemes( theme = "grey_dark" ),
        # While plot is being created a loading screen spinner will appear
        withSpinner(
            plotlyOutput("plot")
            ),
        withSpinner(
            plotlyOutput("candleStick")
            )
                       
    ) 

                     
                     dashboardSidebar(
                       # User Will choose stock Here
                       selectizeInput("chooseStock", choices = NULL, label = "Choose a stock", selected = stockNames[1]),
                       
                       # User can choose date range for stock
                       dateRangeInput(
                         "chooseDate",
                         label = "Choose a Date"
                       )
                       
                     ),
                     
                     dashboardBody( 
                       
                       shinyDashboardThemes( theme = "grey_dark" ),
                       
                       plotlyOutput("plot"), 
                       
                       plotlyOutput("candleStick")
                     )

)



server <- function(input, output, session) {
  
  # Updates stock choices and date selections in server
  updateSelectizeInput(session, "chooseStock", choices = stockNames, selected = stockNames[1], server = T)
  updateDateRangeInput(session, "chooseDate",  start = start, end = Sys.Date(), max = Sys.Date())
  
  
  # Creating variable for stock information. The '<<-' is to make the variable global
  chosenStock <- reactive( {
    stockInfo <<- getSymbols(input$chooseStock, src = "yahoo", from = input$chooseDate, end = input$chooseDate, 
                             auto.assign = FALSE)
    names(stockInfo) <<- clean_names(stockInfo)
    stockInfo
    

    # Updates stock choices and date selections in server
    updateSelectizeInput(session, "chooseStock", choices = stockNames, selected = stockNames[1], server = T)
    updateDateRangeInput(session, "chooseDate",  start = start, end = Sys.Date(), min = start, max = Sys.Date())
    
    
    # Creating variable for stock information. The '<<-' is to make the variable global
    chosenStock <- reactive( {
        stockInfo <<- getSymbols(input$chooseStock, src = "yahoo", from = input$chooseDate[1], end = input$chooseDate[2], 
                                 auto.assign = FALSE)
        names(stockInfo) <<- clean_names(stockInfo)
        stockInfo
        
    })
    
    # Creating interactive plot for stock at specified range
  output$plot <- renderPlotly ({
    req(chosenStock(), stockInfo)
      P1 <-  chosenStock() %>%
        ggplot(aes(x = index(stockInfo), y = stockInfo[,4], 
                   text = paste("Date: ", date(stockInfo),
                                "<br>Stock Price: $", stockInfo[,4]),
                   group = "Date")) +
        geom_line(color = "orange") + 
        labs( title =  paste("Closing Price for", input$chooseStock),
              y = "Stock Price",
              x = "Date")
      
      ggplotly(P1, tooltip = "text")

  })
  
  # # Creating interactive plot for stock at specified range
  output$plot <- renderPlotly ({
    req(chosenStock(), stockInfo)
    P1 <-  chosenStock() %>% 
      
      ggplot(aes(x = date(stockInfo), y = stockInfo[,4])) +
      geom_line() +
      labs( title =  paste("Closing Price for", input$chooseStock),
            y = "Stock Price",
            x = "Date")
    ggplotly(P1)

  })
  
  output$candleStick <- renderPlotly({
    req(chosenStock(), stockInfo)

    
    dat <- as.data.frame(stockInfo)
    dat$date <- index(stockInfo)
    dat <- subset(dat, date >= "2016-01-01")
    
    fig <- plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open,
                   colors = c("red", "forestgreen"), hoverinfo = "none") 
    fig <- fig %>% add_segments(y = ~Low, yend = ~High, size = I(1)) 
    fig <- fig %>% add_segments(y = ~Open, yend = ~Close, size = I(3)) 
    fig <- fig %>% layout(showlegend = FALSE, yaxis = list(title = "Price")) 
    fig <- fig %>% rangeslider()
    fig
  })

    
    dat <- as.data.frame(stockInfo)
    dat$date <- index(stockInfo)
    dat <- subset(dat, date >= "2016-01-01")
    
    fig <- plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open,
                   colors = c("red", "forestgreen"), hoverinfo = "none") 
    fig <- fig %>% add_segments(y = ~Low, yend = ~High, size = I(1)) 
    fig <- fig %>% add_segments(y = ~Open, yend = ~Close, size = I(3)) 
    fig <- fig %>% layout(showlegend = FALSE, yaxis = list(title = "Price")) 
    fig <- fig %>% rangeslider()
    fig
  })
  
  
}

shinyApp(ui, server)

