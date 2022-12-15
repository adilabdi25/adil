#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)

mobilitiy <- read.csv("movement_data.csv",sep = ";")
mobilitiy$Date <- as.Date(mobilitiy$Date)#as.Date() method returns the object of a class "Date"
mobilitiy$Province <- as.factor(mobilitiy$Province) #returns the original object of a class with the requested column specified as a factor rather than a numeric
# Define UI for application that will show a simple project
ui <- fluidPage(
  titlePanel("Population by age"),
  sidebarLayout(
  sidebarPanel(
    
    selectInput(inputId = "dv",label="category", 
                choices = c("children and young adolescents", "The working-age population", "The elderly population","Youth and old age dependicies across the world", "ageing and health", "median age across the globe"),
                selected = "the elderly population"),
    
    selectInput(inputId = "provinces","Province(s)", 
                choices = levels(mobilitiy$Province),
                multiple = TRUE,
                selected =  "The working-age population", "The elderly population","Youth and old age dependicies across the world"),
    dateRangeinput(inputId = "date", label = "Date Range",
                   start = min (mobilitiy$Date),
                   end = max (mobilitiy$date))
    ),
   mainPanel(
     plotOutput (outputId = "plot"),
     em ("Positive and negative percentages indicate an increase and decrease from aseline period(median value between january 3 to february 2) respectively. "),
    
      DT::dataTableOutput(outputId = "table")
     
    )
   )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered_data <- reactive({
    subset(mobility,
           province %in% input$provinces &
             Date >= input$date[1] & Date <= input$date[2])
  })
  
  output$plot <- renderPlot({
    ggplot(filter_data(),
           aes_string(x="Date", y=input$dv, colour="province")) + geom_point(alpha = 0.5) +
      ylab("%change from baseline")
  })
  
  
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)
