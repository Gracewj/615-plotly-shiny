library(plotly)
library(shiny)
library(magrittr)
library(dplyr)

#Divide into four parts: total/rep/dom/ind
#Total donate amount data
data <- read.csv("11-5 MASSCONTRIBUTIONS-csv.csv")
state<- read.csv("states.csv")
data$date <- as.Date(data$date,"%Y-%m-%d")
data %<>% select(cycle,city,state,date,amount,party)
total_donate <- data %>% 
  group_by(state) %>% 
  summarise(sum_donate=sum(amount))
colnames(total_donate)[1]<-"st_abrev"
total_donate %<>% inner_join(y = state,by = "st_abrev")

#Republican donate amount data
data %>% filter(party=="R") %>% group_by(state) %>% 
  summarise(sum_donate=sum(amount)) -> rep_amount
colnames(rep_amount)[1]<-"st_abrev"
rep_amount %<>% inner_join(y = state,by = "st_abrev")

#Democrate donate amount data
data %>% filter(party=="D") %>% group_by(state) %>% 
  summarise(sum_donate=sum(amount)) -> dem_amount
colnames(dem_amount)[1]<-"st_abrev"
dem_amount %<>% inner_join(y = state,by = "st_abrev")

#Independent donate amount data 
data %>% filter(party=="I") %>% group_by(state) %>% 
  summarise(sum_donate=sum(amount)) -> ind_amount
colnames(ind_amount)[1]<-"st_abrev"
ind_amount %<>% inner_join(y = state,by = "st_abrev") 



ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)



server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_ly(z = total_donate$sum_donate, text = total_donate$st_name, 
            locations = total_donate$st_abrev,
            type = 'choropleth', locationmode = 'USA-states') %>%
      colorbar(title = "USD") %>% 
      layout(geo = g)
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view event data" else d
  })
  
}

shinyApp(ui, server)



####
#slider reference: https://plot.ly/r/sliders/
# button reference: https://plot.ly/r/dropdowns/


