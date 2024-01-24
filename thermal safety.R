#calculating thermal safety margins from bootstrap values and leaf temps

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(xlsx)
library(gridExtra)

#read in data - bootstraps
heating_data<-read_excel("data/boot_1000.xlsx")
#read in leaf temps
leaf<-read_excel("data/leaf_temperatures.xlsx")

heating_data<-heating_data%>%separate_wider_delim(id,names=c("Date","State","Species"),delim=".")
heating_data$Date2<-ymd(heating_data$Date)
#keep just 2022 and 2023
heating_data<-heating_data%>%
  filter(lubridate::year(Date2)%in%c(2022,2023))
#keep only June and July
heating_data<-heating_data%>%
  filter(lubridate::month(Date2)%in%c(6,7))
heating_data$year<-year(heating_data$Date2)

#Determine hottest leaf temperature per species per year
leaf_max<-leaf%>%
  group_by(species,year)%>%
  drop_na(temp)%>%
  summarise(max=max(temp))
colnames(leaf_max)[1]<-"Species"

#subtract leaf temp from tcrit for each species for each year ----
#keeping months separate within a year
heating_data<-merge(heating_data,leaf_max,by=c("Species","year"),all=T)
#remove NA
heating_data<-heating_data%>%
  drop_na(tcrit)
#calculate difference between max leaf temp and crit values
#a positive is where the crit value > leaf temp, a neg is crit value < leaf temp
#note overwriting the original Tcrit values
heating_data2<-heating_data%>%
  mutate(across(tcrit:T95,~.x-max,.names="{col}_safety"))

#start to create some plots from these summary ----
#begin with looking at just mean and se by year and month
heating_data2$month<-as.factor(lubridate::month(heating_data2$Date2))
new<-heating_data2%>%
  group_by(Species,year,month)%>%
  summarise(across(tcrit_safety:T95_safety,list(mean=mean,sd=sd,se=~sd(.)/sqrt(1000))))

new$year<-as.factor(new$year)
levels(new$year)
levels(new$month)
ggplot(new,aes(x=tcrit_safety_mean,y=Species,color=month))+
  geom_point(position=position_dodge(width=1))+
  geom_errorbar(aes(xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd),
                position=position_dodge(width=1))+
  geom_vline(xintercept=0)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),axis.line=element_line(colour="black"))+
  facet_wrap(~year)
