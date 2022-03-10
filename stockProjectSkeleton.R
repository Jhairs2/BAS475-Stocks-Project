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
stockNames <- stockSymbols()[,1:2]
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
                         
                         
                       ),
                       
                       #Making menu tabs
                       sidebarMenu( id = "tabs",
                                    menuItem("Plots", tabName = "tab1", icon = icon("fas fa-chart-bar")),
                                    menuItem("Ticker Lookup", tabName = "tab2", icon = icon("info-circle"))
                                    
                       )
                       
                     ),
                     
                     dashboardBody(
                       shinyDashboardThemes( theme = "grey_dark" ),
                       
                       
                       # Setting content for tabs
                       tabItems(
                         tabItem("tab1", 
                       # While plot is being created a loading screen spinner will appear
                                 withSpinner(
                                   plotlyOutput("plot")
                                 ),
                                 
                                 withSpinner(
                                   plotlyOutput("candleStick")
                                 )),
                         
                         tabItem("tab2", 
                                
                                 dataTableOutput("lookup")
                         )
                        
                         
                       ) 
                       
                     )
)

tabnames <- c("tab1, tab2")

server <- function(input, output, session) {
  
  #Keeps track of tabs
  active <- reactiveValues(tab = 1)
  
  # Updates stock choices and date selections in server
  updateSelectizeInput(session, "chooseStock", choices = stockNames$Symbol, selected = stockNames$Symbol[1], server = T)
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
    req(input$chooseStock)
    P1 <-  chosenStock() %>%
      ggplot(aes(x = index(stockInfo), y = stockInfo[,4], 
                 text = paste("Date: ", date(stockInfo),
                              "<br>Stock Price: $", stockInfo[,4]),
                 group = "Date")) +
      geom_line(color = "orange") + 
      labs( title =  paste("Closing Price for", input$chooseStock),
            y = "Stock Price",
            x = "")
    
    ggplotly(P1, tooltip = "text")
    
  })
  
  output$candleStick <- renderPlotly({
    req(input$chooseStock)
    
    
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
  
  #Shows stock ticker and name for reference
  output$lookup <- renderDataTable({
    stockNames
  })
  
}

shinyApp(ui, server)

