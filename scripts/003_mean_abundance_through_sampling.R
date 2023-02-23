#--DESCRIPTION--#
#Author: SLM
#Created: 23022023
#Updated: 23022023

#Part3 of a R dashboard that displays several key useful statistics for the Living and Playing Together Project

#How does abundance change with tides?


#--LIBRARIES--#
library(tidyverse) #data wrangling - data viz
library(ggthemes) #ggplot2 themes
#library(patchwork)
library(ggtext) #add and modify text to ggpplot
library(ggridges) #plotting ridges 
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()


#-- READ TRACKING DATA--#
tracking_data <- read_csv('https://www.dropbox.com/s/3zqqr5qjair49gb/wrdataset2.csv?dl=1') #from SLM's PhD

count_abundance <- tracking_data %>%
    group_by(time,tide, site, species) %>%
    count() #%>%
    #filter(species =='Herring'|
    #species=='Mangrove Jack'|
    #species=='Moses Perch'|
    #species =='YellowfinBream')

mean_abundance <- count_abundance %>%
    mutate(time = round(time)) %>%
    group_by(site, species, time) %>%
    summarise(mean_abundance = mean(n)) %>%
    filter(time >6 & time<17) %>%
    filter(species=='YellowfinBream' | species=='Moses Perch')

colour_palette <- c('#00A6FB', '#F3752B' )
plot <- ggplot(mean_abundance, aes(x = mean_abundance , y = as.factor(time), fill = species)) +
    geom_density_ridges(alpha = 0.7, scale = 0.5, rel_min_height = 0.01, position = position_points_jitter(height=0.1, seed =125))+
    scale_fill_manual(labels =c('Moses perch', 'Yellowfin bream'), values = colour_palette)+
    theme(legend.position = c(0.9, 0.5)) +
    labs(
        fill="Species",
        x= "Mean abundance",
        y = "Time",
        title = "Automated detection of fish individuals using FishID",
        subtitle = "Fast, automatic detection of recreationally important species can be achieved with FishID. FishID improves monitoring of estuaries <br>
                    across south-east Queensland. This <i> ridge graph</i> visualises the mean abundance of moses perch and yellowfin bream across time. <br>
                    Automated monitoring allows for efficient understanding of aquatic habitats and help inform management and conservation.",
        caption = "Visualization by S Lopez Marcano  •  Project by FishID team at Griffith University  • Abundance values from SLM's CV models.")

ggsave('outputs/pdfs/abundance_through_time.pdf', width =13, height = 15)
