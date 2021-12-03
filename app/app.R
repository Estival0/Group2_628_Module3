
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(shinyWidgets)
library(png)

header <- dashboardHeader(title = "COVID-19 & Ice Cream Shops in the US")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data", tabName = "data"),
    menuItem("Key Findings", tabName = "results"),
    menuItem("Business Suggestions", tabName = "suggestions")
  ),
  sidebarMenu(
    selectInput("city", "Choose a state/province:",
                choices = c('BC', 'CO', 'FL', 'GA', 'MA', 'OH', 'OR', 'TX', 'WA', 'WY'))
  )
  
)

body <- dashboardBody(
  setBackgroundImage(
    src = "icstock.jpeg",
    shinydashboard = TRUE
  ),
  tabItems(
    tabItem(tabName = "data",
            h2("Data Description"),
            img(src = "dates.png", height = '300px', width = '500px')
            
    ),
      tabItem(tabName = "results",
              h2("Key Findings")
              
      ),   
    tabItem(tabName = "suggestions",
            h2("Business Suggestions")
    )
  ),
)


footer = dashboardFooter(
  left = "Questions about this information? Contact mcquaig@wisc.edu",
  right = NULL
)

ui <- dashboardPage(header,
                    sidebar, 
                    body,
                    controlbar = NULL,
                    footer)

server <- function(input, output, session) {

  

}


shinyApp(ui = ui, server = server)

