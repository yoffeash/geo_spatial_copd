### shiny app ### 

library(shiny)
library(tidyverse)
library(reshape2)
library(ggmap)
library(zoo)
library(plotly)

# map_key <- "AIzaSyDAwVOtaV9r-yRyJtOxOYgaf--q4pv18hg"
# map1 <- google_map(key=map_key, location='united states', zoom=4)
map1 <- get_googlemap(center=c(lon=-96.5795, lat=39.8283), zoom=4, key="AIzaSyDAwVOtaV9r-yRyJtOxOYgaf--q4pv18hg", maptype="roadmap")
exac_grouped_ninety <- read_csv("data/exacerbations_grouped_ninety.csv")

# Define UI ----
ui <- fluidPage(
 
  titlePanel("Map of Exacerbations by 90 Day Increments"),
 
 sidebarLayout(
   sidebarPanel(   
     fluidRow(
            sliderInput("slider1", h3("Time"),
                        min = as.Date("2006-03-01"), max = as.Date("2010-02-08"), value = as.Date("2006-03-01"), step=90, animate = animationOptions(interval=3000, loop=TRUE)))),
   
   mainPanel(h3("Map"),
             textOutput("selected_month"),
             plotOutput("selected_map", width = 1000, height = 900)))
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_month <- renderText({ 
    paste("You have selected", input$slider1, "to", input$slider1 + 90)
  })
  
  output$selected_map <- renderPlot({ 
    mapPoints <- ggmap(map1) + geom_point(aes(x=longitude, y=latitude, size=total_exac_percent, color = mean_severity, fill = mean_severity),
                                         data=subset(exac_grouped_ninety,ninetyday==input$slider1), alpha=0.6, na.rm=TRUE) + 
                         scale_fill_gradient2(low="orange", mid="red", high="purple", midpoint=2) + 
                         scale_color_gradient2(low="orange", mid="red", high="purple", midpoint=2) + scale_size(range=c(0,30))
    mapPoints
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)