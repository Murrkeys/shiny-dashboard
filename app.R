#
# Shiny App to analyze and visualize average DRG cost data for top 100 DRG in 
# the United States.
# 
# Author : Murray Keogh
# Update Date : 9/25/2020
#

library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

#set working director
setwd("~/General/Personal Projects/R/DRG_Price")

#read in data
data <- read.csv("DRG_Cost.csv")

#set columns as factors
data <- data %>% 
            mutate(DRG.Definition = as.factor(DRG.Definition),
                   Provider.State = as.factor(Provider.State),
                   Provider.City = as.factor(Provider.City)
                   ) %>% drop_na()
        



ui <- dashboardPage(
  
    dashboardHeader(title = "Healthcare Costs"),
    
    
    dashboardSidebar(
      
      
      
      #put in DRG lookup
      pickerInput(
        inputId = 'drg_input', label = 'Search for DRG below:', choices = levels(data$DRG.Definition),
        options = list(`actions-box` = TRUE),multiple = T,selected = levels(data$DRG.Definition)
      ),
      
      #put in State lookup
      pickerInput(
        inputId = 'state_input', label = 'Search for State below:', choices = levels(data$Provider.State),
        options = list(`actions-box` = TRUE),multiple = T,selected = levels(data$Provider.State)
      )),

    
    dashboardBody(
      
      fluidRow(
      
      #put in 5 "metric boxes"
      
      #number of discharges
      valueBoxOutput("num_dc_Box",width=2),
      
      #average covered charges
      valueBoxOutput("avg_charge_Box",width=2),
      
      #median covered charges
      valueBoxOutput("median_charge_Box",width=2),
      
      #standard deviation
      valueBoxOutput("sd_charge_Box",width=2),
      
      #IQR
      valueBoxOutput("iqr_charge_Box",width=2)
      ),
      
      #put in histogram
      fluidRow(
        box(plotOutput("hist", height = 500)),
        box(plotOutput("bar_chart",height = 500))
      )
    
  )
)

server <- function(input, output) {
  
  #filter based on inputs
  data_subset <- reactive({
    filter(data,DRG.Definition == input$drg_input & Provider.State == input$state_input) 
  })
  
  #calculate total discharges
  
  #create metric boxes
  output$num_dc_Box <- renderValueBox({
    valueBox(
      sum(data_subset()$Total.Discharges), "Number of Discharges",
      color = "blue",icon = icon("user-check")
    )
  })
  
  output$avg_charge_Box <- renderValueBox({
    valueBox(
      paste('$',formatC(mean(data_subset()$Average.Covered.Charges),digits = 2,big.mark=',',format = 'f')), "Average Charges",
      color = "red"
    )
  })
  
  output$median_charge_Box <- renderValueBox({
    valueBox(
      paste('$',formatC(median(data_subset()$Average.Covered.Charges),digits = 2,big.mark=',',format = 'f')), "Median Charges",
      color = "green"
    )
  })
  
  output$sd_charge_Box <- renderValueBox({
    valueBox(
      paste('$',formatC(sd(data_subset()$Average.Covered.Charges),digits = 2,big.mark=',',format = 'f')), "Standard Deviation Charges",
      color = "yellow"
    )
  })
  
  output$iqr_charge_Box <- renderValueBox({
    valueBox(
      paste('$',formatC(IQR(data_subset()$Average.Covered.Charges),digits = 2,big.mark=',',format = 'f')), "IQR Charges",
      color = "orange"
    )
  })
  
  #create histogram of charges
  
  output$hist <- renderPlot({
    
    # Render the plot
    ggplot(data_subset(), aes(x=Average.Covered.Charges)) + geom_histogram()
  })


#create bar chart of state differences

output$bar_chart <- renderPlot({
  
  # Render the plot
  ggplot(data_subset(), aes(x=Provider.State,y=Average.Covered.Charges)) + stat_summary(fun = "mean", geom = "bar")
  })
}


shinyApp(ui, server)


