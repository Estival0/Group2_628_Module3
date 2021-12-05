
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(shinyWidgets)
library(png)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(reticulate)
#Sys.setenv(RETICULATE_PYTHON = "/Library/Frameworks/Python.framework/Versions/3.8/bin/python3")
#Renviron is here: /Library/Frameworks/R.framework/Versions/4.0/Resources/etc

py_install("pandas")
use_python('/Users/maritmcquaig/Library/r-miniconda/envs/r-reticulate/bin/python', require=T)
source_python("/Users/maritmcquaig/Documents/GitHub/Group2_628_Module3/generate_suggestions.py")
R.home()

#shop <- 'The Waffle Window'
#city <- 'Portland'
#period <- "pre covid"
#state <- "OR"
#suggestions = main_func(shop, city, state, period)
#length(suggestions)
#good_suggestion = suggestions[0:1]
#bad_suggestion = suggestions[2:2]

#plot
c <- read.csv("/Users/maritmcquaig/Documents/stat628/module3/covid.csv")
pc <- read.csv("/Users/maritmcquaig/Documents/stat628/module3/precovid.csv")
pc$date <- format(as.POSIXct(pc$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
pcdates <- pc %>% count(date)
c$date <- format(as.POSIXct(c$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
cdates <- c %>% count(date)
pcdates <- pcdates %>% add_column(covid = "Pre-Covid-19")
cdates <- cdates %>% add_column(covid = "Covid-19")
alldates <- rbind(pcdates,cdates)



header <- dashboardHeader(title = "COVID-19 & Ice Cream Shops in the US")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data", tabName = "data"),
    menuItem("Key Findings", tabName = "results"),
    menuItem("Business Suggestions", tabName = "suggestions")
  ),
  sidebarMenu(
    selectInput("city", "Choose a state/province:",
                choices = c('BC', 'CO', 'FL', 'GA', 'MA', 'OR', 'TX'))
  )
  
)

body <- dashboardBody(
  setBackgroundImage(
    src = "icstock.jpeg",
    shinydashboard = TRUE
  ),
  tabItems(
    tabItem(tabName = "data",
            h2("Yelp Reviews of Ice Cream Shops Since March 2019"),
            fluidRow(box(plotOutput("plot1", height = 250)),
                     box(h5("All conclusions presented on this site come from Yelp reviews left from March 2019 through January 2021. Since shutdowns associated with Covid-19 began in the United States around mid-march, we split our dataset into two around that time. We also removed a one month window of data (3/1/20-4/1/20), to avoid any confusion in reviews left around the very early stages of the pandemic and to account for different states closing down at different rates. ")))
            
    ),
      tabItem(tabName = "results",
              h2("Key Findings")
              
      ),   
    tabItem(tabName = "suggestions",
            h2("Business Suggestions")
    )
  )
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
  output$plot1 <- renderPlot({
    ggplot(alldates)+
      geom_point(aes(x = as.Date(date), y = n, col = covid))+
      geom_vline(xintercept = as.numeric(as.Date('2020-03-01')), col = "black")+
      geom_vline(xintercept = as.numeric(as.Date('2020-04-01')), col = "black")+
      xlab("Date")+
      ylab("Number of Reviews")+
      labs(col = "Time Period")
  })
}


shinyApp(ui = ui, server = server)

