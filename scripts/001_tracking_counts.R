#--DESCRIPTION--#
#Author: SLM
#Created: 21022023
#Updated: 21022023

#Part1 of a R dashboard that displays several key useful statistics for the Living and Playing Together Project

#How many fish were tracked in the PhD monitoring?


#--LIBRARIES--#
library(tidyverse) #data wrangling - data viz
library(ggthemes) #ggplot2 themes
#library(patchwork)
library(ggtext) #add and modify text to ggpplot
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()

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





#-- PLOT --#
plot <- ggplot(rounded_count, aes(x = species, y = counts)) +
  geom_segment(aes(xend = species, yend = 0), size = 2, color="#4FB477") +
  geom_point(size = 8, color = "#306c48") +
  scale_y_continuous(limits = c(0, max(rounded_count$counts) + 1)) +
  facet_grid('site', labeller = labeller(site=c("NINETEENTH" = "South Gold Coast", "PARADISE" = "North Gold Coast", "TWIN" = "Sunshine Coast"))) +
  coord_flip()+
  scale_x_discrete(labels=c('Yellowfin bream', 'Moses perch', 'Mangrove jack', 'Southern herring'))+
    labs(
        x= "Species",
        y = "Fish Tracks (Thousands)",
        title = "Automated tracking of fish individuals using FishID",
        subtitle = "Behavioural data collected with automated technologies is changing our understanding of fish populations in estuaries. <br>
                    Several species of commercial importance were tracked to understand their behaviour in urbanised estuaries.<br>
                     The <i>lollipop graph</i> visualizes the number of fish tracks (thousands) obtained across several days and  sites. <br>
                    Herring individuals were the most abundant species with FishID processing more than 20,000 tracks in Sunshine Coast.",
        caption = "Visualization by S Lopez Marcano  •  Project by FishID team at Griffith University  • Tracking values from SLM's tracking method.")

#--SAVING PLOT--#
ggsave('outputs/pdfs/tracking_counts.pdf', width=12, height=15)


