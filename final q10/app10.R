library(tidyverse)
library(shiny)

d = read_csv('all-states-history.csv')

d <- d %>% filter(d$state %in% c('CA','FL','TX','MA','NY','OH'))


variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("COVID-19 by Data Quality"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(inputId = "qual", label = "Select Data Quality Grade",
                         choices = names(table(d$dataQualityGrade)), inline = TRUE
      ),
      
      selectInput(
        inputId ="var1",
        label = "Select a Numerical Variable (This is used for line plot)",
        choices = variables_names, selected = "positive"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Numerical Variable",
        choices = variables_names, selected = "death"
      ),
      
      sliderInput(inputId = "date",
                  "Select Date Range:",
                  min = min(d$date, na.rm=TRUE),
                  max = max(d$date, na.rm=TRUE),
                  value = c(Sys.Date()-180,Sys.Date())
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
    
    v1= input$var1
    v2= input$var2
    d <- d %>% filter(date>input$date[1], date<input$date[2])
    d <- d %>% filter(d$dataQualityGrade %in% input$qual)
    
    if(input$plot_choice == 'line')
      
    {
      ggplot(d, aes(x = date, y=d[[v1]],color = d$dataQualityGrade))+
        geom_line()+
        labs(x = 'date',color=d$dataQualityGrade)
    }
    
    else
    {
      ggplot(d, aes(x=d[[v2]], y = d[[v1]], color = d$dataQualityGrade))+
        geom_point()+
        labs(x = v1 ,y=v2,color= d$dataQualityGrade)
    }
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)