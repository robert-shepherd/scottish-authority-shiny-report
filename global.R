#Script 2/5: Global options for shiny app

library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(rmarkdown)
library(knitr)

#Optional: If working directory has changed since file was created use the below to
#set the wd to the location of the scottish neighbourhoods file
# setwd("C:/Users/rshepherd/Desktop/MSc/R Programming/Project")

#Read in data
scottish_neighbourhoods <- read.csv("scottish_neighbourhoods.csv") 

#Amending field names
names(scottish_neighbourhoods)[4] <- "Crime"
names(scottish_neighbourhoods)[5] <- "Dwellings CT band A-C (% of total)"
names(scottish_neighbourhoods)[6] <- "Dwellings CT band D-E (% of total)"
names(scottish_neighbourhoods)[7] <- "Dwellings CT band F-H (% of total)"
names(scottish_neighbourhoods)[8] <- "Hospital admissions"
names(scottish_neighbourhoods)[9] <- "House prices"
names(scottish_neighbourhoods)[10] <- "Job seekers allowance (% on jsa)"
names(scottish_neighbourhoods)[11] <- "Municipal waste"
names(scottish_neighbourhoods)[12] <- "Municipal waste (% of total collected)"
names(scottish_neighbourhoods)[13] <- "Proximity to derelict site (% living close)"
names(scottish_neighbourhoods)[16] <- "Local Authority"

#Unique ref areas for ui
ref_area_type <- sort(unique(scottish_neighbourhoods$ref_area_type),decreasing = TRUE)

#Variables available for each ref area
variables_by_ref <- scottish_neighbourhoods[c(14,4:13)]
variables_gathered <- gather(variables_by_ref,"variable","value", -ref_area_type,na.rm = TRUE)
unique_variables <- unique(variables_gathered[1:2])

#Default spatial units
default_spatial <- scottish_neighbourhoods %>%
  filter(ref_area_type == "Local Authority" & !is.na(Crime)) %>%
  select(ref_area_desc) %>%
  distinct %>%
  arrange(ref_area_desc)
