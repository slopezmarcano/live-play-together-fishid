#--DESCRIPTION--#
#Author: SLM
#Created: 21022023
#Updated: 21022023

#Part2 of a R dashboard that displays several key useful statistics for the Living and Playing Together Project

#--LIBRARIES--#
library(tidyverse) #data wrangling - data viz
library(ggthemes) #ggplot2 themes
#library(patchwork)

library(showtext)
font_add_google("Lato")
showtext_auto()
#--READ MULTIOCCUPANCY MODEL DATA--#
data_w_predictions <- read_csv("https://www.dropbox.com/s/7b348bgjetwk5u1/data_w_predictions.csv?dl=1")%>%
  mutate(day=as.character(day))

#-- FILTER USEFUL DATA FROM DATASET--#
data2 <- data_w_predictions %>%
    filter(site=='TWIN' & day=='2021-01-25'|
           site== 'NINETEENTH' & day=='2021-04-15'|
           site== 'PARADISE' & day== '2021-05-10')


a_occup <- data2 %>%
  mutate(time = format(as.POSIXct(data2$interval1), format = "%H:%M"))%>%
  filter(site=='TWIN')%>%
  select(!c(X,...1))%>%
  group_by(time, site)%>%
  summarise(mean=mean(pred),
         sd=sd(pred))
a_occup$time <- as.POSIXct(a_occup$time, format = "%H:%M") 
  
  ggplot(a_occup, aes(x=time, y=mean))+ 
  #geom_point(size=3)+
  theme_clean(base_family = "Lato")+
  geom_rect(xmin = as.POSIXct('2023-02-22 06:22'), xmax = as.POSIXct('2023-02-22 10:00'), 
            ymin = -Inf, ymax = Inf, fill = '#8181cf', alpha = 0.1) +
  geom_rect(xmin = as.POSIXct('2023-02-22 10:02'), xmax = as.POSIXct('2023-02-22 12:00'), 
            ymin = -Inf, ymax = Inf, fill = '#8181cf', alpha = 0.1) +
  geom_rect(xmin = as.POSIXct('2023-02-22 12:02'), xmax = as.POSIXct('2023-02-22 17:00'), 
            ymin = -Inf, ymax = Inf, fill = '#8181cf', alpha = 0.1)+
geom_path(aes(group=site), colour='red', size=4)+
    geom_text(x = as.POSIXct('2023-02-22 06:00:00'), y = 1.03, label = "6AM-10AM", hjust = 0, size =10) +
  geom_text(x = as.POSIXct('2023-02-22 10:02:00'), y = 1.03, label = "10AM-12PM", hjust = 0, size=10) +
  geom_text(x = as.POSIXct('2023-02-22 14:00:00'), y = 1.03, label = "12PM-4PM", hjust = 0, size=10)+
  theme(axis.ticks.x = element_blank())+ #axis.text.x = element_blank()
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  ylab('Probability') + theme(plot.title = element_text(size = 18)) +labs(title = "Site A")+labs(x = "Time", y = "Mean probability", 
    colour = "Sampling days")