library(plotly)
library(shiny)
library(magrittr)
library(dplyr)
library(zoo)

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
rep_amount %<>% mutate(party=rep("Republican",dim(rep_amount)[1]))

#Democrate donate amount data
data %>% filter(party=="D") %>% group_by(state) %>% 
  summarise(sum_donate=sum(amount)) -> dem_amount
colnames(dem_amount)[1]<-"st_abrev"
dem_amount %<>% inner_join(y = state,by = "st_abrev")
dem_amount %<>% mutate(party=rep("Democrats",dim(dem_amount)[1]))

#Republican & Democrate together
both_amount <- inner_join(dem_amount,rep_amount, by=c("st_abrev","st_name"))


#Independent donate amount data 
data %>% filter(party=="I") %>% group_by(state) %>% 
  summarise(sum_donate=sum(amount)) -> ind_amount
colnames(ind_amount)[1]<-"st_abrev"
ind_amount %<>% full_join(y = state,by = "st_abrev")  
ind_amount %<>% mutate(party=rep("Independent",dim(ind_amount)[1]))
ind_amount$st_name.x <- NULL
colnames(ind_amount)[4] = "st_name"
ind_amount$sum_donate[is.na(ind_amount$sum_donate)] = 0
colnames(ind_amount)[4] <- "party"
ind_amount <- ind_amount[c("st_abrev", "sum_donate", "st_name", "party")]
ind_amount<-ind_amount[-9,]

# Total data including party
total_party<-rbind(dem_amount,ind_amount,rep_amount)

# We take a log of the donate money for better visual display

total_party$sum_donate <- log(total_party$sum_donate+1)
#Min of data and Max of data
minDate <- as.Date("2016-06-05","%Y-%m-%d")
maxDate <- as.Date("2018-10-17","%Y-%m-%d")


ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  
  # titlePanel("Contribution Map"),
  # 
  # sidebarLayout(      
  # 
 
  
  selectInput(inputId = "party",label = "Data",choices=c("Democrats","Republican","Independent"),
               helpText("Choose the data you want to see in the map"),multiple = FALSE)
  
  # mainPanel(
  #   plotOutput("plot")  
  # )
  # 

  )

#combine data into one data frame to choose?

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
    
  
      
    plot_ly(z = total_party$sum_donate[total_party$party == input$party], text = total_donate$st_name, 
            locations = total_donate$st_abrev,
            type = 'choropleth', locationmode = 'USA-states',
            colorscale='PuBu') %>%
      colorbar(title = "USD in log") %>% 
      layout(geo = g)
  })

  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view contribution amount" else d
  }) 
    
}
shinyApp(ui, server)



####
#slider reference: https://plot.ly/r/sliders/
# button reference: https://plot.ly/r/dropdowns/


