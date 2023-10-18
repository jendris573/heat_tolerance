library(readxl)
library(writexl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(tidyverse)
library(pracma)
library(multcomp)
library(ggplot2)


#Load farm weather station data
farm <- read_excel("data/farm_weather_data.xlsx")

#keep only temperature and date columns
farm <- farm[,1:2]

#create column for year
farm <- mutate(farm, year=year(farm$mtSampTime))

#create column for month
farm <- mutate(farm, month=month(farm$mtSampTime))

## create column for julian date##
farm$julian_date <- yday(farm$mtSampTime)

## convert F to C 
farm <- mutate(farm, tempC=5/9*(farm$mtTemp-32))

#######################################
### Determine highest temp by month ###
#######################################

# high temp for May 2023
may_TMAX <- farm %>%
  filter(year == 2023) %>%
  filter(month==5) %>%
  summarise(temp = max(tempC, na.rm = TRUE))

# high temp for June 2023
june_TMAX <- farm %>%
  filter(year == 2023) %>%
  filter(month==6) %>%
  summarise(temp = max(tempC, na.rm = TRUE))

# high temp for July 2023
july_TMAX <- farm %>%
  filter(year == 2023) %>%
  filter(month==7) %>%
  summarise(temp = max(tempC, na.rm = TRUE))

# high temp for August 2023
aug_TMAX <- farm %>%
  filter(year == 2023) %>%
  filter(month==8) %>%
  summarise(temp = max(tempC, na.rm = TRUE))

# high temp for September 2023 
sep_TMAX <- farm %>%
  filter(year == 2023) %>%
  filter(month==9) %>%
  summarise(temp = max(tempC, na.rm = TRUE))

#########################################
### Compare WWTP temperature readings ###
#########################################

## Data prep

#Load NOAA Climate Data Online data
tenn_clim<-read.csv("data/Tennessee_climate.csv")

#keep only sewage plant
tenn_clim <- tenn_clim%>%filter(STATION=="USC00401790")

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

#omit NA in TMAX recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,9]),]


## Calculate Absolute High Temperatures 

#record high temp for May 2023
may2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==5) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June 2023
june2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for July 2023
july2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==7) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for August 2023
aug2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==8) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for September 2023
sep2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))


##########################################
### Create dataframe and write to XLSX ###
##########################################

#create an empty dataframe
monthly_TMAX <- as.data.frame(matrix(NA, nrow=5, ncol=2))

#Label rows with month names
rownames(monthly_TMAX) <- c("May", "June", "July", "August", "September")

#column name
colnames(monthly_TMAX) <- c("farm_temp", "wwtp_temp")

#add monthly farm TMAX to df
monthly_TMAX[1,1] = may_TMAX
monthly_TMAX[2,1] = june_TMAX
monthly_TMAX[3,1] = july_TMAX
monthly_TMAX[4,1] = aug_TMAX
monthly_TMAX[5,1] = sep_TMAX

#add monthly WWTP TMAX to df
monthly_TMAX[1,2] = may2023_TMAX
monthly_TMAX[2,2] = june2023_TMAX
monthly_TMAX[3,2] = july2023_TMAX
monthly_TMAX[4,2] = aug2023_TMAX
monthly_TMAX[5,2] = sep2023_TMAX
