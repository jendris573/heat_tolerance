#test script to compare critical temps to threshold temperature values

#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(xlsx)
library(gridExtra)
library(car)
library(stringr)

#read in data
heating_data<-read_excel("data/boot_1000.xlsx")

#clean and organize data
#separate the ID column
#Separate back out the date from the ID column
heating_data$date<-substr(heating_data$id, 1, 10)

#Separate back out the state from the ID column
heating_data$state<-substr(heating_data$id, 12, 13)

#remove the date from the ID column
heating_data$id<-str_sub(heating_data$id, 15, )

#convert these new columns back to numeric
heating_data$date<-as.Date(heating_data$date)

#separate date column into day/month/year
heating_data<-heating_data %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))

#test signficance of bootstrapped outputs against specific thresholds
#climate data is coming from NOAA and thresholds determined in a separate scripts
########All time record high###########
#load the mean and CI critical values for these same species so you can compare
crits<-read_excel("data/crit_values.xlsx")

#All-time high in Clarksville TN since 1900 is 44.4
#start with just a simple test of different species in June 2022 versus this threshold
test<-heating_data%>%
  filter(id=="Juglans nigra")%>%
  filter(month==6)%>%
  filter(!is.na(tcrit))

thresh<-38.3

mean(test$tcrit>=thresh)
