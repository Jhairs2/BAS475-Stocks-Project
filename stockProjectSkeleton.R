library(shiny)
library(shinydashboard)
library(fpp3)
library(quantmod)
library(ggplot2)
library(plotly)

# Getting stock symbols and creating date range
stockNames <- stockSymbols()[,1]
start <- as.Date("2010-01-01")
end <- as.Date("2022-03-01")

ui <- dashboardPage(
    
    dashboardHeader(),
    
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
        
        plotlyOutput("plot")
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
        
    })
    
    # Creating interactive plot for stock at specified range
    output$plot <- renderPlotly ({
      P1 <-  chosenStock() %>%
        ggplot(aes(x = date(stockInfo), y = stockInfo[,4])) +
            geom_line() + 
            labs( title =  paste("Closing Price for", input$chooseStock),
                                y = "Stock Price",
                                x = "Date")
      ggplotly(P1)
      
    })
    
    
}

shinyApp(ui, server)

