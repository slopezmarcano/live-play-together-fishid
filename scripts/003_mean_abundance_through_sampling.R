#--DESCRIPTION--#
#Author: SLM
#Created: 23022023
#Updated: 23022023

#Part3 of a R dashboard that displays several key useful statistics for the Living and Playing Together Project

#How does abundance change throughout the day and with tides?


#--LIBRARIES--#
library(tidyverse) #data wrangling - data viz
#library(gganimate) #animations
library(ggthemes) #ggplot2 themes
#library(patchwork)
library(ggtext) #add and modify text to ggpplot
library(ggridges) #plotting ridges 
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()


#-- READ TRACKING DATA--#
count_abundance <- read_parquet("data/converted_parquet/output_file.parquet") %>% #useful resources:https://hbs-rcs.github.io/large_data_in_R/
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


#--SETTING THEMES, FONTS AND COLOUR PALETTE--#
#--SETTING THEME AND FONTS--#
theme_set(theme_clean(base_family = "Lato"))

theme_update(
  # Set size 20 and colour for both y and x axes
  axis.title = element_text(color = "#404E4D", size = 20),
  # Axes labels are greyish
  axis.text = element_text(color = "#404E4D"),
  strip.text = element_text(colour = "#404E4D", size=20),
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
  strip.background = element_rect(fill='grey98', color = "grey98"),
  panel.border = element_rect(color='#e1dfdf', fill= NA, size=1),
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
  legend.position = "none",
  legend.background = element_rect(fill = 'grey98', color = NA), 
  legend.title = element_text(color = "#404E4D", size= 20),
  legend.text =  element_text(color = "#404E4D", size= 18))

#--COLOUR PALETTE--#
colour_palette <- c('#00A6FB', '#F3752B' )

#--PLOT--#
plot <- ggplot(mean_abundance, aes(x = mean_abundance , y = as.factor(time), fill = species)) +
    geom_density_ridges(alpha = 0.7, scale = 0.5, rel_min_height = 0.01, position = position_points_jitter(height=0.1, seed =125))+
    scale_fill_manual(labels =c('Moses perch', 'Yellowfin bream'), values = colour_palette)+
    theme(legend.position = c(0.9, 0.5)) +
    labs(
        fill="Species",
        x= "Mean abundance",
        y = "Time")

#--PLOT LABELS--#
plot <- plot + ggtitle("Automated detection of fish individuals using FishID") +
        labs(subtitle = "Fast, automatic detection of recreationally important species can be achieved with FishID. FishID improves monitoring of estuaries <br>
                    across south-east Queensland. This <i> ridge graph</i> visualises the mean abundance of moses perch and yellowfin bream across time. <br>
                    Automated monitoring allows for efficient understanding of aquatic habitats and help inform management and conservation.")+
        labs (caption = "Visualization by S Lopez Marcano  •  Project by FishID team at Griffith University  • Abundance values from SLM's CV models.")

ggsave('outputs/pdfs/abundance_through_time.pdf', width =13, height = 15)

#--ANIMATED PLOT --#
#p_animated <- plot + transition_layers(
  #layer_length = 1,  # duration of each layer in seconds
  #transition_length = 1  # duration of transition between layers in seconds
#)

#anim_save("my_animation.gif", p_animated, fps = 1)
