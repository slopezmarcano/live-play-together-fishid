#--DESCRIPTION--#
#Author: SLM
#Created: 21022023
#Updated: 21022023

#Part1 of a R dashboard that displays several key useful statistics for the Living and Playing Together Project

#How do fish move in the water column?

#--LIBRARIES--#
library(tidyverse) #data wrangling - data viz
library(ggthemes) #ggplot2 themes
#library(patchwork)
library(ggforce)
library(gridExtra)
library(arrow)
library(ggtext) #add and modify text to ggpplot
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()

#NOT POSSIBLE BECAUSE XMIN AND YMIN HAVE BEEN SCALED.REQUIRE RAW DATASE



#-- READ TRACKING DATA--#
data <- read_parquet("data/converted_parquet/output_file.parquet") %>% #useful resources:https://hbs-rcs.github.io/large_data_in_R/
    filter(species=='YellowfinBream') %>%
    select(!c('X.1', tracker_length)) %>%
    mutate(category_id = '1') %>%
    rename(xmin = detection_length, ymin = detection_depth, id = X) %>%
    mutate(width = 1920, height = 1080)

#--FUNCTION CODE--#

annotations_heatmaps <- function (data, category_id2){
  #REQUIREMENTS
  #category_id2: circular id from the annotations files (data)

  #Generate x and y bins
  q <- seq(1,max(data$width), length.out = 40)
  e <- seq(1,max(data$height), length.out = 40)

  #Create combinations between (X and Y) expand grid
  data1 <- expand.grid(X=q, Y=e)

  #Select bin X values
  Xvalues <- data1 %>%
    select(X) %>%
    distinct(X)%>%
    mutate(join=1)

  #Select bin Y values
  Yvalues <- data1 %>%
    select(Y) %>%
    distinct(Y) %>%
    mutate(join=1)

  #Select the detections for one species
  detections <- data %>%
    filter(category_id==1)%>%
    select(xmin, ymin, id)%>% #selecting xmin and ymin of the
    mutate(join=1)

  #Calculate the difference between all detections (x values) and x bin values
  xdifferences <- Xvalues %>%
    full_join(detections, multiple ="all")%>%
    mutate(xdifference=abs(xmin-X))%>%
    group_by(id)%>% #all annotations have a unique ID
    slice(which.min(xdifference)) %>% #select the lowest difference between detection X and x bin value for each annotation
    select(id,X)

  #Calculate the difference between all detections (y values) and y bin values
  ydifferences <- Yvalues %>%
    full_join(detections, multiple = "all")%>%
    mutate(ydifference=abs(ymin-Y))%>%
    group_by(id)%>% #all annotations have a unique ID
    slice(which.min(ydifference)) %>% #select the lowest difference between detection Y and Y bin value for each annotation
    select(id,Y)

  combined_x_y <- full_join(xdifferences, ydifferences) #combine the x and y differences

  #Output of the function.
  counts <- combined_x_y %>%
    group_by(X,Y)%>%
    summarise(n = n())%>% #counts per bin
    ungroup() %>%
    mutate(freq = (n / sum(n)))%>% #calculate the frequency of annotations per bin
    add_row(X=max(data$width), Y=max(data$height), n =0, freq=0)%>% #add fake row to ensure that margins are close to the height and width of the largest image in the dataset
    mutate(category_id2=paste0(1))

 return(counts)
}

test <- annotations_heatmaps(data, 1)

data %>%
group_by(X) %>%
count() %>%
filter(n>=2)