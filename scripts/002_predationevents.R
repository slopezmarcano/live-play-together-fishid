#--DESCRIPTION--#
#Author: SLM
#Created: 21022023
#Updated: 21022023

#Part2 of a R dashboard that displays several key useful statistics for the Living and Playing Together Project

#How many predation events were quantified in the PhD sampling

#--LIBRARIES--#
library(tidyverse) #data wrangling - data viz
library(ggthemes) #ggplot2 themes
#library(patchwork)
library(ggtext) #add and modify text to ggpplot
library(showtext) #fonts
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

#-- FURTHER DATA WRANGLING - SITE SELECTION AND MEAN PROBABILITY OF ENCOUNTERS--#
a_occup <- data2 %>%
  mutate(time = format(as.POSIXct(data2$interval1), format = "%H:%M"))%>%
  filter(site=='TWIN')%>%
  select(!c(X,...1))%>%
  group_by(time, site)%>%
  summarise(mean=mean(pred),
         sd=sd(pred))

#--MODIFYING TIME SO IT CAN PLOTED--#
a_occup$time <- as.POSIXct(a_occup$time, format = "%H:%M") #TODO: this will add today's time - warning!

#--SETTING THEME AND FONTS--#
theme_set(theme_clean(base_family = "Lato"))

theme_update(
  # Set size 20 and colour for both y and x axes
  axis.title = element_text(color = "#404E4D", size = 20),
  # Axes labels are greyish
  axis.text = element_text(color = "#404E4D"),
  # Set the size of the axes labels and margins.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 20, margin = margin(r = 5)),
  # Also, the ticks have a very dark grey color
  axis.ticks = element_line(color = "#333d3d", size = .5),
  # The length of the axis ticks is increased.
  #axis.ticks.length.x = unit(1.3, "lines"),
  #axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 15, 20, 15),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "#404E4D", 
    size = 30, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "#656363", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "#656363", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40)), # Large margin on the top of the caption.
  # Remove legend
  legend.position = "none")


#--PLOTTING--#
plot <- ggplot(a_occup, aes(x=time, y=mean))+ 
    geom_rect(xmin = as.POSIXct('2023-02-23 06:00'), xmax = as.POSIXct('2023-02-23 09:00'), #TODO: these values might not be displayed because its adding today's time
            ymin = -Inf, ymax = Inf, fill = '#eeeded', colour="#e1dfdf", linetype="dotted") +
    geom_rect(xmin = as.POSIXct('2023-02-23 09:10'), xmax = as.POSIXct('2023-02-23 12:00'), 
            ymin = -Inf, ymax = Inf, fill = '#eeeded', colour="#e1dfdf", linetype="dotted") +
    geom_rect(xmin = as.POSIXct('2023-02-23 12:10'), xmax = as.POSIXct('2023-02-23 15:00'), 
            ymin = -Inf, ymax = Inf, fill = '#eeeded', colour="#e1dfdf", linetype="dotted")+
    geom_rect(xmin = as.POSIXct('2023-02-23 15:10'), xmax = as.POSIXct('2023-02-23 17:00'), 
            ymin = -Inf, ymax = Inf, fill = '#eeeded', colour="#e1dfdf", linetype="dotted") +       
    geom_path(aes(group=site), colour='#A63A50', size=4)+
    geom_text(x = as.POSIXct('2023-02-23 06:50:00'), y = 1.03, label = "6AM-9AM", hjust = 0, size =7) +
    geom_text(x = as.POSIXct('2023-02-23 09:50:00'), y = 1.03, label = "9AM-12PM", hjust = 0, size=7) +
    geom_text(x = as.POSIXct('2023-02-23 12:40:00'), y = 1.03, label = "12PM-3PM", hjust = 0, size=7)+
    geom_text(x = as.POSIXct('2023-02-23 15:20:00'), y = 1.03, label = "3PM-5PM", hjust = 0, size=7)+
    theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+
    labs(
        x= "Time Periods",
        y = "Probability of predator-prey encounters",
        title = "Automated tracking of predator-prey events using FishID",
        subtitle = "Predation events are difficult to study, especially underwater, but now FishID makes it possible. The <i>line graph</i> visualizes the probability (1 = high, 0 = low) <br>
                of a predation event ocurring between commercial species (e.g., Mangrove Jack) and prey species (e.g., Herring) in Sunshine Coast. <br>
                The probability of predation events decreases througout the day with its peak occurring during the morning. Predation events are highly dynamic, but <br>
                this data can be used to increase our knowledge on rare behavioural events.",
        caption = "Visualization by S Lopez Marcano  •  Project by FishID team at Griffith University  • Mean values of predator-prey co-ocurrence model.")

#--SAVING PLOT--#
ggsave('outputs/pdfs/predator_prey_probability.pdf', width=15, height=10)
