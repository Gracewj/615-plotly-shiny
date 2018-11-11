library(plotly)
library(shiny)
library(magrittr)
library(dplyr)

#In data cleaning process, we first delete the contributors from the othercountries(outside the USA)
#Then group the data by state
data <- read.csv("/Users/apple/Desktop/MA615 DSR/Class28/11-5 MASSCONTRIBUTIONS-csv.csv")
data$date <- as.Date(data$date,"%Y-%m-%d")
data %<>% select(cycle,city,state,date,amount,party)
total_donate <- data %>% group_by(state) %>% summarise(sum_donate=sum(amount))

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = total_donate, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view event data" else d
  })
  
}

shinyApp(ui, server)