###Leaf Temp  Plots###
###Code written by Joe Endris###

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)
library(reshape2)
library(gridGraphics)

##################################
### Data entry and preparation ###
##################################

leaf_temps <- read_excel("~/Documents/College/02- R code/heating/leaf_temperatures.xlsx")

#create column for julian date
leaf_temps$julian_date <- yday(leaf_temps$date)

#determine mean leaf temp
mean_temp <- leaf_temps%>%
  group_by(species,date)%>%
  dplyr::summarise(leaf_temp_mean= mean(temp))

#####################################################
### Plot to compare highest leaf temp to air temp ###
#####################################################

#Acer saccharum
maple <- filter(leaf_temps, species == 'Acer saccharum')

maple_plot <- ggplot(maple, aes(x=date, y=temp))+
  geom_point()+
  xlab("Date")+
  ylab("Critical Temperature")+
  theme_bw()

maple_plot
