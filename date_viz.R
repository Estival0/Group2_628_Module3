library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

c <- read.csv("/Users/maritmcquaig/Documents/stat628/module3/covid.csv")
pc <- read.csv("/Users/maritmcquaig/Documents/stat628/module3/precovid.csv")

pc$date <- format(as.POSIXct(pc$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
pcdates <- pc %>% count(date)
c$date <- format(as.POSIXct(c$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
cdates <- c %>% count(date)

pcdates <- pcdates %>% add_column(covid = "Pre-Covid-19")
cdates <- cdates %>% add_column(covid = "Covid-19")

alldates <- rbind(pcdates,cdates)
ggplot(alldates)+
  geom_point(aes(x = as.Date(date), y = n, col = covid))+
  geom_vline(xintercept = as.numeric(as.Date('2020-03-01')), col = "black")+
  geom_vline(xintercept = as.numeric(as.Date('2020-04-01')), col = "black")+
  xlab("Date")+
  ylab("Number of Reviews")+
  labs(col = "Time Period")

ggsave("dates.png", width = 10, height = 6)

