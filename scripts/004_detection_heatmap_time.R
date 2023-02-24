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

https://arrow.apache.org/docs/r/reference/read_parquet.html
https://hbs-rcs.github.io/large_data_in_R/

#-- READ TRACKING DATA--#
fhvhv_ds <- parquet_file("data/converted_parquet")

test<-fhvhv_ds %>%
    filter(species=='YellowfinBream') %>%
    mutate(category_id = '1') %>%
    rename(xmin = detection_length,
    ymin= detection_length)

test2<- as.data.frame(test)
#--FUNCTION CODE--#

annotations_heatmaps <- function (category_id2){
  #REQUIREMENTS
  #category_id2: circular id from the annotations files (annos)

  #Generate x and y bins
  q <- seq(1,max(annos$width), length.out = 40)
  e <- seq(1,max(annos$height), length.out = 40)

  #Create combinations between (X and Y) expand grid
  data <- expand.grid(X=q, Y=e)

  #Select bin X values
  Xvalues <- data %>%
    select(X) %>%
    distinct(X)%>%
    mutate(join=1)

  #Select bin Y values
  Yvalues <- data %>%
    select(Y) %>%
    distinct(Y) %>%
    mutate(join=1)

  #Select the detections for one species
  detections <- annos %>%
    filter(category_id==category_id2)%>%
    select(xmin, ymin, id)%>% #selecting xmin and ymin of the
    mutate(join=1)

  #Calculate the difference between all detections (x values) and x bin values
  xdifferences <- Xvalues %>%
    full_join(detections)%>%
    mutate(xdifference=abs(xmin-X))%>%
    group_by(id)%>% #all annotations have a unique ID
    slice(which.min(xdifference)) %>% #select the lowest difference between detection X and x bin value for each annotation
    select(id,X)

  #Calculate the difference between all detections (y values) and y bin values
  ydifferences <- Yvalues %>%
    full_join(detections)%>%
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
    add_row(X=max(annos$width), Y=max(annos$height), n =0, freq=0)%>% #add fake row to ensure that margins are close to the height and width of the largest image in the dataset
    mutate(category_id2=paste0(category_id2))

 return(counts)
}

