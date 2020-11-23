library(tidyverse)
library(shiny)

d = read_csv('all-states-history.csv')

d <- d %>% filter(d$state %in% c('CA','FL','TX','MA','NY','OH'))


variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("COVID-19 by States"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(inputId = "state", label = "Select State(s)",
                         choices = names(table(d$state)), inline = TRUE
      ),
      
      selectInput(
        inputId ="var1",
        label = "Select a Numerical Variable",
        choices = variables_names, selected = "positive"
      ),
      
      sliderInput(inputId = "date",
                  "Select Date Range:",
                  min = min(d$date, na.rm=TRUE),
                  max = max(d$date, na.rm=TRUE),
                  value = c(Sys.Date()-30,Sys.Date())
      ),
      
      radioButtons(inputId = "plot_choice", 
                   label = h3("Select Plot:"),
                   choices = c("Line Plot" = "line",
                               "Point Plot" = "point"),
                   selected = 'line'
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
    
    d <- d %>% filter(date>input$date[1], date<input$date[2])
    d <- d %>% filter(d$state %in% input$state)
    
    if(input$plot_choice == 'line')
      
    {
      ggplot(d, aes(x = date, y=d[[input$var1]],color = d$state))+
        geom_line()+
        labs(x = input$var1,color=d$state)
    }
    
    else
    {
      ggplot(d, aes(x=date, y = d[[input$var1]], color = d$state))+
        geom_point()+
        labs(x = input$var1 ,color= d$state)
    }
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)