### shiny app ### 

library(shiny)
library(tidyverse)
library(reshape2)
library(ggmap)
library(zoo)
library(plotly)
library(scales)

map1 <- get_googlemap(center=c(lon=-96.5795, lat=39.8283), zoom=4, key="AIzaSyDAwVOtaV9r-yRyJtOxOYgaf--q4pv18hg", maptype="roadmap")
exac_grouped_ninety_placebo <- read_csv("data/exacerbations_grouped_ninety_placebo.csv")
exac_grouped_ninety_azithro <- read_csv("data/exacerbations_grouped_ninety_azithro.csv")

# Define UI ----
ui <- fluidPage(
 
  titlePanel("Map of Exacerbations by 90 Day Increments"),
 
  fluidRow(
    column(6, align="center",
           sliderInput("slider1",  
                       h3("Time"),
                       min = as.Date("2006-08-28"), max = as.Date("2009-11-10"), value = as.Date("2006-08-28"), step=90, animate = animationOptions(interval=3000, loop=TRUE))),
    column(6, align="center",
           h3(textOutput("selected_month")))
    ),
  
    fluidRow(
      column(6, align="center",
             h3("Placebo"),
             plotOutput("selected_map_placebo", width = 1000, height = 900)),
      column(6, align="center",
             h3("Azithromycin"),
             plotOutput("selected_map_azithro", width = 1000, height = 900))
    )
   
)
  
# Define server logic ----
server <- function(input, output) {
  
  output$selected_month <- renderText({ 
    paste("You have selected", input$slider1, "to", input$slider1 + 90)
  })
  
  output$selected_map_placebo <- renderPlot({ 
    mapPoints <- ggmap(map1) + geom_point(aes(x=longitude, y=latitude, size=total_exac_percent, color = mean_severity),
                                         data=subset(exac_grouped_ninety_placebo,ninetyday==input$slider1), alpha=0.6, na.rm=TRUE) + 
      scale_color_gradient2(low="orange", mid="red", high="purple", midpoint=2, limits=c(1,4)) + 
      scale_size_continuous(limits=c(0,100), range=c(0,40)) +
      labs(colour="Severity of\nExacerbation", size="Exacerbation\nRate") +
      guides(colour = guide_colorbar(order = 1), 
             shape = guide_legend(order = 2)) +
      theme(legend.title = element_text(face="bold", size=20)) +
      theme(legend.text = element_text(size=14)) +
      theme(legend.direction = "horizontal") +
      theme(legend.position = c(0,0), legend.justification = c(0,0)) +
      theme(legend.background = element_rect(fill=alpha('grey',0.7))) +
      theme(legend.key = element_blank()) 
    mapPoints
  })
  
  output$selected_map_azithro <- renderPlot({ 
    mapPoints <- ggmap(map1) + geom_point(aes(x=longitude, y=latitude, size=total_exac_percent, color = mean_severity),
                                          data=subset(exac_grouped_ninety_azithro,ninetyday==input$slider1), alpha=0.6, na.rm=TRUE) + 
      scale_color_gradient2(low="orange", mid="red", high="purple", midpoint=2, limits=c(1,4)) + 
      scale_size_continuous(limits=c(0,100), range=c(0,40)) +
      labs(colour="Severity of\nExacerbation", size="Exacerbation\nRate") +
      guides(colour = guide_colorbar(order = 1), 
             shape = guide_legend(order = 2)) +
      theme(legend.title = element_text(face="bold", size=20)) +
      theme(legend.text = element_text(size=14)) +
      theme(legend.direction = "horizontal") +
      theme(legend.position = c(0,0), legend.justification = c(0,0)) +
      theme(legend.background = element_rect(fill=alpha('grey',0.7))) +
      theme(legend.key = element_blank()) 
    mapPoints
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)