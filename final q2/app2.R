library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')

variables_names = c('Survived','Pclass','Name','Sex')

ui <- fluidPage(
  
  titlePanel("Bar Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select your first Categorical Variable",
        choices = variables_names, 
        selected = "Pclass"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Categorical Variables",
        choices = variables_names,
        selected = "Sex"
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Bar Plot----
      
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    d = read_csv('titanic.csv')
    v1 = input$var1
    v2 = input$var2
   
    
    library(ggplot2)
      
    r = ggplot(d, aes(x = d[[v1]], fill = as.factor(d[[v2]])))+
        geom_bar()+
        labs(x = v1, fill = v2)
      
    return(r)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)