library(tidyverse)
library(shiny)
library(lubridate)

d = read_csv('AB_US_2020.csv')

d$last_review <- dmy(d$last_review)

variables_names = names(d)

ui <- navbarPage("Navigation Bar!",
           tabPanel("AirBnb Data with Custom Plots- Bar/Line",
                    
                    sidebarLayout(
                      sidebarPanel(
                        
                        checkboxGroupInput(inputId = "city", label = "Select City or Cities",
                                           choices = names(table(d$city)), inline = TRUE
                        ),
                        
                        selectInput(
                          inputId ="var1",
                          label = "Select a Categorical Variable: ",
                          choices = variables_names, selected = "room_type"
                        ),
                        selectInput(
                          inputId = "var2",
                          label = "Select a Numerical Variable",
                          choices = variables_names, selected = "price"
                        ),
                        
                        sliderInput(inputId = "date",
                                    "Select Date Range for Last Review:",
                                    min = min(d$last_review, na.rm=TRUE),
                                    max = max(d$last_review, na.rm=TRUE),
                                    value = c(min(d$last_review),max(d$last_review))
                        ),
                        
                        radioButtons(inputId = "plot", 
                                     label = h3("Select Plot:"),
                                     choices = c("Line Plot" = "line",
                                                 "Bar Plot" = "bar"),
                                     selected = 'bar'
                        ),
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = 'show_plot')
                      )
                    )
           ),
           
           
           tabPanel("Airbnb Data with Custom Plots - Histogram",
                    
                    sidebarLayout(
                      sidebarPanel(
                        
        
                          checkboxGroupInput(inputId = "cities", label = "Select City or Cities",
                                             choices = names(table(d$city)), inline = TRUE
                          ),
                          
                          selectInput(
                            inputId ="var3",
                            label = "Select a Numerical Variable: ",
                            choices = variables_names, selected = ""
                          ),
                          
                          selectInput(
                            inputId ="var4",
                            label = "Select a Categorical Variable: ",
                            choices = variables_names, selected = ""
                          ),
                          
                          
                          sliderInput(inputId = "dates",
                                      "Select Date Range for Last Review:",
                                      min = min(d$last_review, na.rm=TRUE),
                                      max = max(d$last_review, na.rm=TRUE),
                                      value = c(min(d$last_review),max(d$last_review))
                          ),
                          
                          radioButtons(inputId = "yesno", 
                                       label = h3("Subplot city?:"),
                                       choices = c("Yes"= 'Yes',"No"= 'No'),
                                       selected = 'No'
                          ),
              
              
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = 'show_plot2')
                      )
                    )
           )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    v2 = input$var2
    
    
    d <- d %>% filter(last_review>input$date[1], last_review<input$date[2])
    d <- d %>% filter(d$city %in% input$city)
    
    library(ggplot2)
    
    if(input$plot == 'bar')
    {
      ggplot(d, aes(x = d[[v1]],y= d[[v2]], color=d$city))+
        geom_col()+
        labs(x = v1, y= v2, color= d$city)
    }
    else
    {
      ggplot(d, aes(x= last_review, y=d[[v2]], color= d$city))+
        geom_line()+
        labs(x= 'date',y= v2, color= d$city)+
        facet_wrap(~ d[[v1]])
    }
    
  })
  
  output$show_plot2 <- renderPlot({
    
    v3 = input$var3
    v4 = input$var4
   

    
    d <- d %>% filter(last_review>input$dates[1], last_review<input$dates[2])
    d <- d %>% filter(d$city %in% input$cities)
    
    library(ggplot2)
    
    if(input$yesno == 'No'){
      
      ggplot(d, aes(x = d[[v3]], color = d[[v4]]))+
        geom_histogram()+
        labs(x = v3, color= v4)
    }
    else{
      ggplot(d, aes(x = d[[v3]], color = d[[v4]]))+
        geom_histogram()+
        labs(x = v3, color = v4) + facet_wrap(~ d$city)
    }
    
  })
  
}
# app
shinyApp(ui = ui, server = server)