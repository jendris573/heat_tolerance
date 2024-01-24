#calculating thermal safety margins from bootstrap values and leaf temps

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(xlsx)
library(gridExtra)
library(ggridges)#needed for density plots

#read in data - bootstraps
heating_data<-read_excel("data/boot_1000.xlsx")
#read in leaf temps
leaf<-read_excel("data/leaf_temperatures.xlsx")
################################
#some data cleaning----
################################
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

################################
#calculate leaf thermal safety margins
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

###################Plotting########################################
#start to create some plots from these summary ----
#begin with looking at just mean and se by year and month
heating_data2$month<-as.factor(lubridate::month(heating_data2$Date2))
new<-heating_data2%>%
  group_by(Species,year,month)%>%
  summarise(across(tcrit_safety:T95_safety,list(mean=mean,sd=sd,se=~sd(.)/sqrt(1000))))

new$year<-as.factor(new$year)
levels(new$year)
levels(new$month)
#Good lord this is ugly
ggplot(new,aes(x=tcrit_safety_mean,y=Species,color=month))+
  geom_point(position=position_dodge(width=1))+
  geom_errorbar(aes(xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd),
                position=position_dodge(width=1))+
  geom_vline(xintercept=0)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),axis.line=element_line(colour="black"))+
  facet_wrap(~year)

#sort it from descending in June for each year separate ----
#2022
ggplot()+
  geom_point(data=new%>%filter(month==6&year==2022),aes(x=tcrit_safety_mean,y=reorder(Species,-tcrit_safety_mean),color=month),size=3,
             position = position_nudge(y = 0.1))+
  geom_errorbar(data=new%>%filter(month==6&year==2022),aes(x=tcrit_safety_mean,y=Species,xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd),
                color="black",
                position = position_nudge(y = 0.1),width=0.25)+
  geom_point(data=new%>%filter(month==7&year==2022),aes(x=tcrit_safety_mean,y=Species),color="gray40",shape=17,size=3,
             position = position_nudge(y = -0.1))+
  geom_errorbar(data=new%>%filter(month==7&year==2022),aes(x=tcrit_safety_mean,y=Species,xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd),
                color="gray40",position = position_nudge(y = -0.1),width=0.25)+
  geom_vline(xintercept=0)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),axis.line=element_line(colour="black"))+
  scale_color_manual(name="Month",values=c("black","gray40"),lables=c("June","July"))

#2022
g1<-ggplot()+
  geom_point(data=new%>%filter(month==6&year==2022),aes(x=tcrit_safety_mean,y=reorder(Species,-tcrit_safety_mean),color=month),size=3,
             position = position_nudge(y = 0.1))+
  geom_errorbar(data=new%>%filter(month==6&year==2022),aes(x=tcrit_safety_mean,y=Species,xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd,color=month),
                position = position_nudge(y = 0.1),width=0.25)+
  geom_point(data=new%>%filter(month==7&year==2022),aes(x=tcrit_safety_mean,y=Species,color=month),size=3,
             position = position_nudge(y = -0.1))+
  geom_errorbar(data=new%>%filter(month==7&year==2022),aes(x=tcrit_safety_mean,y=Species,xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd,color=month),
                position = position_nudge(y = -0.1),width=0.25)+
  geom_vline(xintercept=0)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),axis.line=element_line(colour="black"),legend.position = "none",axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.85,vjust=-10))+
  scale_color_manual(name="Month",values=c("black","gray50"),labels=c("June","July"))+
  xlab("Thermal safety margin")+
  ggtitle("2022")
g1
#2023
g2<-ggplot()+
  geom_point(data=new%>%filter(month==6&year==2023),aes(x=tcrit_safety_mean,y=reorder(Species,-tcrit_safety_mean),color=month),size=3,
             position = position_nudge(y = 0.1))+
  geom_errorbar(data=new%>%filter(month==6&year==2023),aes(x=tcrit_safety_mean,y=Species,xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd,color=month),
                position = position_nudge(y = 0.1),width=0.25)+
  geom_point(data=new%>%filter(month==7&year==2023),aes(x=tcrit_safety_mean,y=Species,color=month),size=3,
             position = position_nudge(y = -0.1))+
  geom_errorbar(data=new%>%filter(month==7&year==2023),aes(x=tcrit_safety_mean,y=Species,xmin=tcrit_safety_mean-tcrit_safety_sd,xmax=tcrit_safety_mean+tcrit_safety_sd,color=month),
                position = position_nudge(y = -0.1),width=0.25)+
  geom_vline(xintercept=0)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),axis.line=element_line(colour="black"),
        legend.position = c(0.8, 0.6),axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.85,vjust=-10))+
  scale_color_manual(name="Month",values=c("black","gray50"),labels=c("June","July"))+
  xlab("Thermal safety margin")+
  ggtitle("2023")
g2
grid.arrange(g1,g2,ncol=2)

#With a density curve instead of calculated mean ----
#I'm liking this
#create new variable to help with sorting - this first sorts by month and then by tcrit within month
#manually determine the order of the species based on the mean values
#For 2022
sp2022<-new%>%filter(year==2022&month==6)%>%
  arrange(desc(tcrit_safety_mean))
sp2023<-new%>%filter(year==2023&month==6)%>%
  arrange(desc(tcrit_safety_mean))
#reorder the factor levels in the larger dataframe by this ordered dataframe
heating_data2$Species<-factor(heating_data2$Species,levels=sp2022$Species)
levels(heating_data2$Species)
g3<-ggplot(data=heating_data2%>%filter(year==2022),aes(x=tcrit_safety,y=Species,fill=month))+
  geom_density_ridges(scale = 1, alpha=0.5) +
         scale_y_discrete(expand=c(0.01, 0)) +
         scale_x_continuous(expand=c(0.01, 0)) +
         xlab("Thermal safety margin")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),axis.line=element_line(colour="black"),
        legend.position = "none",axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.85,vjust=-5))+
  geom_vline(xintercept=0,linewidth=1)+
  scale_fill_manual(name="Month",values=c("black","gray70"),labels=c("June","July"))+
  ggtitle("2022")

heating_data2$Species<-factor(heating_data2$Species,levels=sp2023$Species)
g4<-ggplot(data=heating_data2%>%filter(year==2023),aes(x=tcrit_safety,y=Species,fill=month))+
  geom_density_ridges(scale = 1, alpha=0.5) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0.01, 0)) +
  xlab("Thermal safety margin")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),axis.line=element_line(colour="black"),
        legend.position = c(0.85, 0.8),axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.85,vjust=-5))+
  geom_vline(xintercept=0,linewidth=1)+
  scale_fill_manual(name="Month",values=c("black","gray70"),labels=c("June","July"))+
  ggtitle("2023")
  
grid.arrange(g3,g4,ncol=2)
    