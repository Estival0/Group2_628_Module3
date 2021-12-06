
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
use_python('/Users/violaliu/opt/anaconda3/bin/python', require=T)
source_python("/Users/violaliu/Desktop/hw/generate_suggestions.py")



#shop <- 'The Waffle Window'
#city <- 'Portland'
#period <- "pre covid"
#state <- "OR"
suggestions = main_func(shop, city, state, period)
length(suggestions)
good_suggestion = suggestions[0:1]
bad_suggestion = suggestions[2:2]

#c <- read.csv("/Users/maritmcquaig/Documents/stat628/module3/covid.csv")
#pc <- read.csv("/Users/maritmcquaig/Documents/stat628/module3/precovid.csv")

pc$date <- format(as.POSIXct(pc$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
pcdates <- pc %>% count(date)
c$date <- format(as.POSIXct(c$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
cdates <- c %>% count(date)

pcdates <- pcdates %>% add_column(covid = "Pre-Covid-19")
cdates <- cdates %>% add_column(covid = "Covid-19")

alldates <- rbind(pcdates,cdates)
#ggplot(alldates)+
#  geom_point(aes(x = as.Date(date), y = n, col = covid))+
#  geom_vline(xintercept = as.numeric(as.Date('2020-03-01')), col = "black")+
#  geom_vline(xintercept = as.numeric(as.Date('2020-04-01')), col = "black")+
#  xlab("Date")+
#  ylab("Number of Reviews")+
#  labs(col = "Time Period")




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

