###Leaf Temp  Plots###
###Code written by Joe Endris###

library(tidyverse)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(writexl)
library(gridExtra)
library(MuMIn)
library(reshape2)
library(gridGraphics)

##################################
### Data entry and preparation ###
##################################

leaf_temps <- read_excel("data/leaf_temperatures.xlsx")

#create column for julian date
leaf_temps$julian_date <- yday(leaf_temps$date)

#################################################
### Calculate Absolute High Leaf Temperatures ###
#################################################

#Determine absolute coldest day by year
leaf_temps$date <- as.Date(leaf_temps$date)
class(leaf_temps$date)

#SM leaf temp 
SM <- leaf_temps %>%
  filter(species== "Acer saccharum") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#TP leaf temp 
TP <- leaf_temps %>%
  filter(species== "Liriodendron tulipifera") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#BW leaf temp 
BW <- leaf_temps %>%
  filter(species== "Juglans nigra") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#SB leaf temp 
SB <- leaf_temps %>%
  filter(species== "Celtis laevigata") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#AB leaf temp 
AB <- leaf_temps %>%
  filter(species== "Fagus grandifolia") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#SG leaf temp 
SG <- leaf_temps %>%
  filter(species== "Liquidambar styraciflua") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#HHB leaf temp 
HHB <- leaf_temps %>%
  filter(species== "Ostrya virginiana") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#BC leaf temp 
BC <- leaf_temps %>%
  filter(species== "Prunus serotina") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#CO leaf temp 
CO <- leaf_temps %>%
  filter(species== "Quercus montana") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#SRO leaf temp 
SRO <- leaf_temps %>%
  filter(species== "Quercus falcata") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))

#AE leaf temp 
AE <- leaf_temps %>%
  filter(species== "Ulmus americana") %>%
  group_by(year) %>%
  summarise(temp = max(temp, na.rm = TRUE))
