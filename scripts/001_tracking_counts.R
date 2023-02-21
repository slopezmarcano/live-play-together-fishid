#--DESCRIPTION--#
#Author: SLM
#Created: 21022023
#Updated: 21022023

#Part1 of a R dashboard that displays several key useful statistics for the Living and Playing Together Project

#--LIBRARIES--#
library(tidyverse) #data wrangling - data viz
library(ggthemes) #ggplot2 themes
#library(patchwork)

#-- READ TRACKING DATA--#
tracking_data <- read_csv('https://www.dropbox.com/s/3zqqr5qjair49gb/wrdataset2.csv?dl=1') #from SLM's PhD

#-- TRACKER COUNTS --#
count_tracker_per_site <- tracking_data %>%
    filter(tracker_length > 200) %>%
    group_by(species, site) %>%
    count() %>%
    filter(species =='Herring'|
    species=='Mangrove Jack'|
    species=='Moses Perch'|
    species =='YellowfinBream')

#-- ROUNDED COUNTS --#
rounded_count <- count_tracker_per_site %>%
    mutate(counts = round(n/1000, 0))

#-- PLOT --#
ggplot(rounded_count, aes(x = species, y = counts)) +
  geom_segment(aes(xend = species, yend = 0), size = 2, color="#f4a261") +
  geom_point(size = 8, color = "#e76f51") +
  scale_y_continuous(limits = c(0, max(rounded_count$counts) + 1)) +
  facet_grid('site', labeller = labeller(site=c("NINETEENTH" = "South Gold Coast", "PARADISE" = "North Gold Coast", "TWIN" = "Sunshine Coast"))) +
  coord_flip()+
  theme_clean(base_family= "Helvetica")+
  labs(x = 'Species', y = 'Fish Tracks (Thousands)')+
  theme(panel.background = element_rect(fill = "#264653"),
        plot.background = element_rect(fill="#264653"),
        axis.text = element_text(color = "#f1faee", size = 14),
        axis.title = element_text(color = "#e9c46a", size = 18),
        plot.title = element_text(color="#f1faee", size = 20),
        strip.background = element_rect(fill='#264653'),
        strip.text = element_text(colour = "#e9c46a", size=16),
        panel.border = element_rect(color='white', fill= NA, size=2.5),
        axis.line.x.bottom = element_line(colour = "white", size=1),
        axis.line.y = element_line(colour = "white", size=1))

ggsave('outputs/tracking_counts.png')




