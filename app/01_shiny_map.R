### shiny app ### 

library(shiny)

# Define UI ----
ui <- fluidPage(
 
  titlePanel("Map of Exacerbations by Quarter"),
 
 sidebarLayout(
   sidebarPanel(   
     fluidRow(
            sliderInput("slider1", h3("Time"),
                        min = as.Date("2006-01-01"), max = as.Date("2010-04-01"), value = as.Date("2006-01-01")))),
   
   mainPanel(h3("Map"),
             textOutput("slider1")))
)

# Define server logic ----
server <- function(input, output) {
  output$slider1 <- renderText({ 
    "You have selected this"
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)