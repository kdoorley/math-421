library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Titanic Final App"),
  
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
        choices = variables_names, selected = "Sex"
      ),
      
      sliderInput(inputId = "Age",
                  "Select Age Range:",
                  min = min(d$Age, na.rm=TRUE),
                  max = max(d$Age, na.rm=TRUE),
                  value= c(.42,80)
      ),
      
      radioButtons(inputId = "plot_choice", 
                   label = h3("Select Plot:"),
                   choices = c("Density Plot" = "density",
                               "Histogram Plot" = "histogram"),
                   selected = 'density'
      ),
  
     checkboxGroupInput(inputId = "Class", label = "Select Class",
                     choices = names(table(d$Pclass)), inline = TRUE
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
    
    v1 = input$var1
    v2 = input$var2
    
    
    library(ggplot2)
    
    d <- d %>% filter(Age>input$Age[1], Age<input$Age[2])
    d <- d %>% filter(d$Pclass %in% input$Class)
    
    if(input$plot_choice == 'density')
      
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