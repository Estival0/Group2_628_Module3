
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
library(rsconnect)

#for reticulate
#use_python('/Library/Frameworks/Python.framework/Versions/3.8/bin/python3', require=T)
source_python("generate_suggestions.py")


#business details
c <- read.csv("covid.csv")
pc <- read.csv("precovid.csv")
biz <- read.csv("clean_business.csv")
cbiz <- c['business_id']
pcbiz <- pc['business_id']
ids <- unique(intersect(cbiz,pcbiz))
biz <- biz[c('business_id','name','address','city','state','stars','review_count')]
cols = c('business_id','name','address','city','state','stars','review_count')
business <- data.frame(matrix(nrow = 0, ncol = length(cols)))
colnames(business) <- cols
for (i in 1:length(ids[,1])){
  business[nrow(business)+1,] = biz[which(biz$business_id == ids[i,1]),]
}


#plot
c <- read.csv("covid.csv")
pc <- read.csv("precovid.csv")
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
  )
  
  
  
)

body <- dashboardBody(
  setBackgroundImage(
    src = "icstock.jpeg",
    shinydashboard = TRUE
  ),
  tabItems(
    tabItem(tabName = "data",
            h2("Yelp Reviews of Ice Cream Shops Since 03/2019"),
            fluidRow(box(plotOutput("plot1", height = 250))),
            fluidRow(box(
              h5("All conclusions presented on this site come from Yelp reviews left from March 2019 through January 2021. Since shutdowns associated with Covid-19 began in the United States around mid-march, we split our dataset into two around that time. We also removed a one month window of data (3/1/20-4/1/20), to avoid any confusion in reviews left around the very early stages of the pandemic and to account for different states closing down at different rates. "),
              title = "Data Description"))
            
    ),
      tabItem(tabName = "results",
              h2("Key Findings")
              
      ),   
    tabItem(tabName = "suggestions",
            h2("Business Suggestions"),
            selectInput("state", "Choose a state/province:",
                        choices = c('BC', 'CO', 'FL', 'GA', 'MA', 'OR', 'TX')),
            uiOutput("city"),
            uiOutput("shop"),
            selectInput("period","Choose a time period:",
                        choices = c("pre covid", "covid")),
            fluidRow(box(tableOutput("good"),title = "What's going right for your shop:")),
            fluidRow(box(tableOutput("bad"),title = "Where your shop can improve:"))
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
  output$city <- renderUI ({
    selectInput("city", "Choose a city:", choices = sort(unique(subset(business,business$state == input$state)$city)))
  })
  output$shop <- renderUI ({
    selectInput("shop","Choose a business:", choices = sort(unique(subset(business,business$state == input$state & business$city == input$city)$name)))
  })
  output$good <- renderTable({
    unname(as.data.frame(main_func(input$shop, input$city, input$state, input$period)[1]))
  })
  output$bad <- renderTable({
    unname(as.data.frame(main_func(input$shop, input$city, input$state, input$period)[2]))
  })
}


shinyApp(ui = ui, server = server)
#options(rsconnect.max.bundle.size=3145728000)
runApp("/Users/maritmcquaig/Documents/GitHub/Group2_628_Module3/shiny")


