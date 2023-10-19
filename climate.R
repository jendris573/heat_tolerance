###Code to determine absolute highs 
###written by Joe Endris

## unless stated, all dates are since 1900

library(readxl)
library(writexl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(tidyverse)
library(pracma)
library(multcomp)
library(ggplot2)

########################
### Data Preparation ###
########################

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

#omit NA in precipitation recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,6]),]
#omit NA in TMAX recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,9]),]
#omit NA in TMIN recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,10]),]

#filter for 1980-present
tenn1980 <- tenn_clim %>%
  filter(year>1979)

############################################
### Calculate Absolute High Temperatures ###
############################################

#Determine absolute hottest day by year
tenn_clim$DATE <- as.Date(tenn_clim$DATE)
class(tenn_clim$DATE)

#record high temperature since 1900
record_TMAX <- tenn_clim %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for May
may_TMAX <- tenn_clim %>%
  filter(month==5) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June
june_TMAX <- tenn_clim %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for July
july_TMAX <- tenn_clim %>%
  filter(month==7) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for August
aug_TMAX <- tenn_clim %>%
  filter(month==8) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for September
sep_TMAX <- tenn_clim %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June 2022
june2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

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

#record high temp for July 2022
july2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==7) %>%
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

#record high temp for September 2022
sep2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for September 2023
sep2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

################################
### Climate Plots start here ###
################################

## Plot for absolute high by year ##

#determine hottest day by year
TN_TMAX <- tenn1980 %>%
  group_by(year) %>%
  summarise(abs_TMAX = max(TMAX))



#create plot for record high by year
record_TMAX_plot <- ggplot(TN_TMAX, aes(x=year, y= abs_TMAX))+
  geom_smooth(method = "lm")+
  geom_point()+
  labs(title = "Annual Highest Temperatures (°C) since 1980",
       subtitle = "Clarksville, TN",
       y= "Temperature °C",
       x= "Year")+
  theme_bw()+
  theme(panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background  = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_blank(),
      axis.text.x=element_text(angle = 45, hjust = 1))

record_TMAX_plot

#number of days above 35C (95F)
days_35 <- tenn1980 %>%
  group_by(year) %>%
  summarise(number=sum(TMAX>34.99))

#plot number of days above 35C
days_35_plot <- ggplot(days_35, aes(x=year, y=number ))+
  geom_point() +
  geom_smooth(stat="smooth",method="lm")+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1))+
  labs(title = "Number of Days Above 35°C",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year")

days_35_plot


#number of days above 32.2C (90F)
days_32 <- tenn1980 %>%
  group_by(year) %>%
  summarise(number=sum(TMAX>32.19))

#plot number of days above 32.2C
days_32_plot <- ggplot(days_32, aes(x=year, y=number ))+
  geom_point() +
  geom_smooth(stat="smooth",method="lm")+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1))+
  labs(title = "Number of Days Above 32.2°C",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year")

days_32_plot


##################################
### Climate statistical models ###
##################################

high_temp_mod <- lm(data=TN_32.2 %>%
                      filter(year>1979), n ~ year)

summary(high_temp_mod)




