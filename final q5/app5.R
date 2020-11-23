library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Bar Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Categorical Variables",
        choices = variables_names, selected = "Pclass"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Categorical Variables",
        choices = variables_names,
        selected = "Sex"
      ), 
      
      sliderInput(inputId = "Age",
                  "Select Age Range:",
                  min = min(d$Age, na.rm=TRUE),
                  max = max(d$Age, na.rm=TRUE),
                  value= c(.42,80))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Bar Plot ----
      
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    v2 = input$var2
    
    
    library(ggplot2)
    
    d <- d %>% filter(Age>input$Age[1], Age<input$Age[2])
    
    ggplot(d, aes(x = as.factor(d[[v1]]), fill = as.factor(d[[v2]])))+
      geom_bar()+
      labs(x = v1, fill = v2)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)