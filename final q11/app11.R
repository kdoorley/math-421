library(tidyverse)
library(lubridate)
library(shiny)

d = read_csv('AB_US_2020.csv')

d$last_review <- dmy(d$last_review)

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("COVID-19 by Data Quality"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(inputId = "room", label = "Select Room Type",
                         choices = names(table(d$room_type)), inline = TRUE
      ),
      
      selectInput(
        inputId ="var1",
        label = "Select a Numerical Variable: ",
        choices = variables_names, selected = "price"
      ),
      
      
      sliderInput(inputId = "date",
                  "Select Date Range for Last Review:",
                  min = min(d$last_review, na.rm=TRUE),
                  max = max(d$last_review, na.rm=TRUE),
                  value = c(min(d$last_review),max(d$last_review))
      ),
      
      radioButtons(inputId = "city", 
                   label = h3("Select City:"),
                   choices = names(table(d$city))
      ),
      
      
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: plot ----
      
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    
    
    library(ggplot2)
    
    v1= input$var1
    d <- d %>% filter(last_review>input$date[1], last_review<input$date[2])
    d <- d %>% filter(d$room_type %in% input$room)
    d <- d %>% filter(d$city %in% input$city)
    
      ggplot(d, aes(x= d$last_review, y = d[[v1]], color = d$room_type))+
        geom_line()+
        labs(x = 'Date of last review' ,y=v1,color= d$room_type)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)