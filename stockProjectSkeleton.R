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
library(regclass)

# Configuring settings as per tidyquant tutorial
options("getSymbols.warning4.0" = FALSE)
options("getSymbols.yahoo.warning" = FALSE)



# Define function to split names on dot and only keep after dot text
clean_names <- function(stocks) {
  split_names = strsplit(names(stocks), split = '.', fixed = TRUE)
  vapply(split_names, function(x)
    x[2], character(1))
}


# Getting stock symbols and creating date range
stockNames <- stockSymbols()[, 1:2]
start <- as.Date("2010-01-01")
end <- Sys.Date()

ui <- dashboardPage(
  skin = "yellow",

  dashboardHeader(title = "Stock Analysis"),

  dashboardSidebar(
# Making menu tabs
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Interactive Plots",
        tabName = "tab1",
        icon = icon("fas fa-chart-bar")
      ),
      menuItem(
        "Ticker Lookup",
        tabName = "tab2",
        icon = icon("info-circle")
      ),
      menuItem("S&P 500", tabName = "tab3", icon = icon("cog")),
      menuItem(
        "Linear Model View",
        tabName = "tab4",
        icon = icon("chart-line")
      ),
      menuItem("What-If", tabName = "tab5", icon = icon("money")),

# User Will choose stock Here
      selectizeInput(
        "chooseStock",
        choices = NULL,
        label = h3("Choose a stock")
      ),

# User can choose date range for stock
      dateRangeInput("chooseDate",
                     label = h3("Choose a Date"))
    )


  ),

  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),


# Setting content for tabs
    tabItems(
      tabItem(
        "tab1",
# While output is being created a loading screen spinner will appear
        withSpinner(color = "orange",
                    plotlyOutput("plot")),

        withSpinner(color = "orange",
                    plotlyOutput("candleStick"))
      ),

      tabItem("tab2",

              dataTableOutput("lookup")),

      tabItem("tab3",
              withSpinner(color = "orange",
                          plotlyOutput("SANDP"))),


      tabItem(
        "tab4",
        withSpinner(color = "orange",
                    fluidRow(plotOutput("compare"))),
        withSpinner(color = "orange",
                    verbatimTextOutput("summary"))
      ),
      tabItem(
        "tab5",
        numericInput("invest", label = "What if you invested", value = 0),
        actionButton("go", label = "invest!", icon = icon("flag")),
        verbatimTextOutput("whatif")
      )


    )

  )
)

tabnames <- c("tab1, tab2", "tab3", "tab4", "tab5")

server <- function(input, output, session) {


  # Updates stock choices and date selections in server
  updateSelectizeInput(
    session,
    "chooseStock",
    choices = stockNames$Symbol,
    selected = stockNames$Symbol[1],
    server = T
  )
  updateDateRangeInput(
    session,
    "chooseDate",
    start = start,
    end = Sys.Date(),
    min = start,
    max = Sys.Date()
  )


  # Creating variable for stock information. The '<<-' is to make the variable global
  chosenStock <- reactive({
    stockInfo <<-
      getSymbols(
        input$chooseStock,
        src = "yahoo",
        from = input$chooseDate[1],
        to = input$chooseDate[2],
        auto.assign = FALSE
      )
    names(stockInfo) <<- clean_names(stockInfo)
    stockInfo




  })

  # Creating interactive plot for stock at specified range
  output$plot <- renderPlotly({
    req(input$chooseStock)
    P1 <- chosenStock() %>%
      ggplot(aes(
        x = index(stockInfo),
        y = stockInfo[, 4],
        text = paste("Date: ", date(stockInfo),
                     "<br>Stock Price: $", stockInfo[, 4]),
        group = "Date"
      )) +
      geom_line(color = "orange") +
      labs(
        title = paste("Changing Stock Price for", input$chooseStock),
        y = "Stock Price",
        x = ""
      ) + theme(plot.title = element_text(hjust = 0.5))

    ggplotly(P1, tooltip = "text")

  })

  output$candleStick <- renderPlotly({
    req(input$chooseStock)


    dat <- as.data.frame(stockInfo)
    dat$date <- index(stockInfo)
    dat <- subset(dat, date >= "2016-01-01")

    fig <-
      plot_ly(
        dat,
        x = ~date,
        xend = ~date,
        color = ~Close > Open,
        colors = c("red", "forestgreen"),
        hoverinfo = "none"
      )
    fig <- fig %>% add_segments(y = ~Low,
                                yend = ~High,
                                size = I(1))
    fig <-
      fig %>% add_segments(y = ~Open,
                           yend = ~Close,
                           size = I(3))
    fig <-
      fig %>% layout(showlegend = FALSE, yaxis = list(title = "Price"))
    fig <- fig %>% rangeslider()
    fig
  })

  #Shows stock ticker and name for reference
  output$lookup <- renderDataTable({
    stockNames
  })
  INTEREST_STOCKS <- getSymbols("^GSPC",
                                src = "yahoo",
                                auto.assign = FALSE)
  INTEREST_STOCKS_DF <- as.data.frame(INTEREST_STOCKS)
  INTEREST_STOCKS_DF$date <- index(INTEREST_STOCKS)
  p <- ggplot(INTEREST_STOCKS_DF, aes(date, GSPC.Close)) +
    geom_line(color = "orange")

  output$SANDP <- renderPlotly({
    ggplotly(p)
  })


  # Creating influence plot from linear model
  output$compare <- renderPlot({
    M <- lm(Close ~ date(stockInfo), data = chosenStock())
    influence_plot(M)

  })
  # Showing summary for linear model
  output$summary <- renderPrint({
    M <- lm(Close ~ date(stockInfo), data = chosenStock())
    print(summary(M))

  })

  # Takes earliest data point in data for selected stock and tells profit that would be made today
  observeEvent(input$go,
               output$whatif <- {
    renderPrint({
      sub3 <- as.data.frame(chosenStock(),
                                          select = c("Close"))
      date(stockInfo$Close)[1]
      totOwned <- round(input$invest / sub3$Close[1], 3)
      totMade <- round(totOwned * sub3$Close[length(sub3$Close)], 2)
      Profit <- totMade - input$invest

      paste(
                     "If you invested $",
                     input$invest,
                     "in",
                     date(stockInfo$Close)[1],
                     "(the earliest recorded date in this app's data for this stock)",
                     "you would own",
                     totOwned,
                     "of",
                     input$chooseStock,
                     "stock! This means that you would have, $",
                     totMade,
                     "today! Your profit would be $",
                     Profit
                   )

    })
  })

}

shinyApp(ui, server)


