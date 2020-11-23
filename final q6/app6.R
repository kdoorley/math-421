library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Density Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Numeric Variables",
        choices = variables_names, selected = "Age"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Numeric Variables",
        choices = variables_names,
        selected = ""
      ),
      
      radioButtons(inputId = "plot_choice", 
                   label = h3("Select Plot:"),
                   choices = c("Box Plot" = "box",
                     "Histogram Plot" = "histogram"),
                   selected = 'box')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    v2 = input$var2
    plt = input$plot_choice
   
    
    library(ggplot2)
    
    if(plt == 'box')
      
    {
      ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
        geom_density()+
        labs(x = v1, color = v2)
    }
    
    else
    {
      ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
        geom_histogram()+
        labs(x = v1, color = v2)
    }
      
      
    
  })
  
}
# app
shinyApp(ui = ui, server = server)