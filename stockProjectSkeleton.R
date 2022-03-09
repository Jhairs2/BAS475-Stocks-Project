library(shiny)
library(shinydashboard)
library(fpp3)
library(quantmod)
library(ggplot2)
library(plotly)

# Getting stock symbols and creating date range
stockNames <- stockSymbols()[,1]
start <- as.Date("2010-01-01")
end <- Sys.Date()

ui <- dashboardPage(
    
    dashboardHeader(),
    
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
     # While plot is being created a loading screen spinner will appear
    withSpinner(
     ( plotlyOutput("plot") 
    ) 
    )
)
)


server <- function(input, output, session) {
    
    # Updates stock choices and date selections in server
    updateSelectizeInput(session, "chooseStock", choices = stockNames, selected = stockNames[1], server = T)
    updateDateRangeInput(session, "chooseDate",  start = start, end = Sys.Date(), min = start, max = Sys.Date())
    
    
    # Creating variable for stock information. The '<<-' is to make the variable global
    chosenStock <- reactive( {
        stockInfo <<- getSymbols(input$chooseStock, src = "yahoo", from = input$chooseDate[1], end = input$chooseDate[2], 
                                 auto.assign = FALSE)
        
    })
    
    # Creating interactive plot for stock at specified range
    output$plot <- renderPlotly ({

      # Graph will not appear until stock has been selected and loaded in server
    req(input$chooseStock)
    
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
    
    
}

shinyApp(ui, server)

