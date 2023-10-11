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
library(nlme)

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

#julian date
heating_data$julian<-yday(heating_data$date)

#separate date column into day/month/year
heating_data<-heating_data %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))

##Trying regressions of tcrit over time for the big 3 species since we have May-Sept data
#filter just 2023 data
test<-heating_data%>%
  filter(year==2023)%>%
  filter(month<9)%>%
  filter(id=="Acer saccharum"|id=="Fagus grandifolia"|id=="Liriodendron tulipifera")%>%
  filter(!is.na(tcrit))

#Tcrit temperature
ggplot(data=test,aes(x=julian,y=tcrit))+
  geom_point()+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")+
  facet_wrap(~id)

#T50 temperature
ggplot(data=test,aes(x=julian,y=T50))+
  geom_point()+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")+
  facet_wrap(~id)

#T95 temperature
ggplot(data=test,aes(x=julian,y=T95))+
  geom_point()+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")+
  facet_wrap(~id)

#individual regression models of critical temperature through time
#looking for potential acclimation
#focusing on Tcrit at first, then T50 and T95 for each species
#all come out as significant but the slope is so shallow and the R2 is so low that it doesn't really matter
mod<-lm(tcrit~julian,data=test%>%filter(id=="Acer saccharum"))
summary(mod)
mod<-lm(T50~julian,data=test%>%filter(id=="Acer saccharum"))
summary(mod)
mod<-lm(T95~julian,data=test%>%filter(id=="Acer saccharum"))
summary(mod)

mod<-lm(tcrit~julian,data=test%>%filter(id=="Fagus grandifolia"))
summary(mod)
mod<-lm(T50~julian,data=test%>%filter(id=="Fagus grandifolia"))
summary(mod)
mod<-lm(T95~julian,data=test%>%filter(id=="Fagus grandifolia"))
summary(mod)

mod<-lm(tcrit~julian,data=test%>%filter(id=="Liriodendron tulipifera"))
summary(mod)
mod<-lm(T50~julian,data=test%>%filter(id=="Liriodendron tulipifera"))
summary(mod)
mod<-lm(T95~julian,data=test%>%filter(id=="Liriodendron tulipifera"))
summary(mod)
###########################################
###########################################
##Trying regressions of tcrit over time for all species in 2023
#filter just 2023 data
test<-heating_data%>%
  filter(year==2023)%>%
  filter(month>5)%>%
  filter(!is.na(tcrit))

#tcrit through time
ggplot(data=test,aes(x=julian,y=tcrit,group=year))+
  geom_point()+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth",aes(group=year))+
  facet_wrap(~id)
#T50 through time
ggplot(data=test,aes(x=julian,y=T50,group=year))+
  geom_point()+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth",aes(group=year))+
  facet_wrap(~id)
#really rough linear model of Tcrit over time with a random effect of species to allow for slope variation
mod<-lme(tcrit~julian,random=~1|id,data=test)
anova(mod)
summary(mod)
#a significant slope but it is so close to zero that ecologically it doesn't make any difference

#nothing seems to show strong acclimation over time when looking at all species

################################################
################################################
#investigating if critical values change from year to year using just june and july data from 2022 and 2023
# #filter just 2023 data
# test<-heating_data%>%
#   filter(month>5&month<9)%>%
#   filter(!is.na(tcrit))

#start with just comparing a single species across years
test<-heating_data%>%
  filter(id=="Celtis laevigata")%>%
    filter(month>5&month<9)%>%
    filter(!is.na(tcrit))

mod<-lm(tcrit~year,data=test%>%filter(month==6))
summary(mod)

mod<-lm(tcrit~year,data=test%>%filter(month==7))
summary(mod)

ggplot(data=test%>%filter(month==7),aes(x=year,y=tcrit,group=year))+
  geom_boxplot()
# mod<-lme(tcrit~month*year,random=~1|id,data=test)
# summary(mod)
# anova(mod)
