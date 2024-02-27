### Heating Tolerance Plots
### code written by Joe Endris
### with guidance from Evan Rehm

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggimage)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)
library(reshape2)
library(gridGraphics)

# # # # # # # # # # # # # # # # #
# Data entry and preparation ----
# # # # # # # # # # # # # # # # #

#read in data sets
tcrit <- read_excel("data/crit_values_final.xlsx")
leaf_max_temp <- read_excel("data/leaf_temperatures.xlsx", sheet =2)

#filter just TN data
tcrit<-tcrit[which(tcrit$state=="TN"),]

#create column for julian date
tcrit$julian_date <- yday(tcrit$date)

#create column for month
tcrit <- mutate(tcrit, month=month(tcrit$date))

#create column for year
tcrit <- mutate(tcrit, year=year(tcrit$date))

#add column for image location - needed for plotting leaf image
leaf_max_temp$location<-'images/leaf_image2.jpg'#change this filename to whatever .jpg or .png - can only use jpg or png
#If using a different .jpg, make sure the file size is really small (<10 KB). Otherwise the plotting
#takes forever and you end up with a huge file size for the ggplot. Not sure why it does this.

#Load NOAA Climate Data Online data
tenn1980<-read_excel("data/tenn1980.xlsx")

#create column for year
tenn1980 <- mutate(tenn1980, year=year(tenn1980$DATE))

#create column for month
tenn1980 <- mutate(tenn1980, month=month(tenn1980$DATE))

## create column for julian date##
tenn1980$julian_date <- yday(tenn1980$DATE)

#omit NA in TMAX/TMIN recordings 
tenn1980<-tenn1980[complete.cases(tenn1980[,9]),]

heat_season <- tenn1980 %>%
  filter(julian_date>152) %>%
  filter(julian_date<273)

# # # # # # # # # # # # # # # # # #
# mean annual high temperature ----
# # # # # # # # # # # # # # # # # #
TN_mean <- heat_season %>%
  group_by(year) %>%
  summarise(mean_TMAX = mean(TMAX))

mean_TMAX_plot <- ggplot(TN_mean, aes(x=year, y= mean_TMAX))+
  geom_point()+
  geom_smooth(method= lm)+
  #scale_y_continuous(limits = c(17.5, 25)) +
  labs(
    y= "Temperature °C",
    x= "Year")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background  = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1))

mean_TMAX_plot

# # # # # # # # # # # # #
# hottest day of year----
# # # # # # # # # # # # #

#determine hottest day by year
TN_TMAX <- heat_season %>%
  group_by(year) %>%
  summarise(abs_TMAX = max(TMAX))

mean_TMAX <- heat_season %>%
  group_by(julian_date) %>%
  summarise(abs_TMAX = max(TMAX))

mean(mean_TMAX$abs_TMAX)

#create plot for record high by year
record_TMAX_plot <- ggplot(TN_TMAX, aes(x=year, y= abs_TMAX))+
  geom_point()+
  geom_smooth(method= lm)+
  #geom_hline(yintercept = 36.58824, color= "red", linewidth = 1.25)+ #mean summer TMAX
  scale_y_continuous(limits = c(33, 44),
                     breaks=seq(32,45,by=2),
                     minor_breaks = seq(32, 45, 1)) +
  labs(
    y= "Temperature °C",
    x= "Year")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background  = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1))

record_TMAX_plot

# # # # # # # # # # # # # # # # 
# number of days above 32.2----
# # # # # # # # # # # # # # # #

#number of days above 32.2C (90F)
days_32 <- heat_season %>%
  group_by(year) %>%
  summarise(number=sum(TMAX>32.19))

#plot number of days above 32.2C
days_32_plot <- ggplot(days_32, aes(x=year, y=number ))+
  geom_point() +
  #geom_hline(yintercept = 54.1, color= "red", linewidth = 1.25)+ #mean number of days above 32.2
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x=element_text(angle = 45, hjust = 1))+
  labs(y= "Number of Days",
       x= "Year")

days_32_plot

# # # # # # # # # # # # # # # #
## facet wrap climate plot ----
# # # # # # # # # # # # # # # #

grid.arrange(mean_TMAX_plot, record_TMAX_plot, days_32_plot,ncol=1)


# # # # # # # # # # # # # # 
# four month Tcrit plot----
# # # # # # # # # # # # # # 

#June 2022
June2022 <- ggplot(tcrit%>%
                     filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 38.3, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2022

July2022 <- ggplot(tcrit%>%
                     filter(year==2022,month==7), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 38.9, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2022")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2022

June2023 <- ggplot(tcrit%>%
                     filter(year==2023,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 38.3, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2023

July2023 <- ggplot(tcrit%>%
                     filter(year==2023,month==7), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 37.2, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  scale_x_discrete(limit=rev)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2023

grid.arrange(June2022,July2022, June2023,July2023,ncol=2)

# # # # # # # # # # # # #
# four month T50 plot----
# # # # # # # # # # # # #

#June 2022
June2022_t50 <- ggplot(tcrit%>%
                     filter(year==2022,month==6), aes(y= T50.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  geom_hline(yintercept = 38.3, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("June 2022")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


July2022_t50 <- ggplot(tcrit%>%
                     filter(year==2022,month==7), aes(y= T50.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  geom_hline(yintercept = 38.9, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("July 2022")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

June2023_t50 <- ggplot(tcrit%>%
                     filter(year==2023,month==6), aes(y= T50.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  geom_hline(yintercept = 38.3, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("June 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


July2023_t50 <- ggplot(tcrit%>%
                     filter(year==2023,month==7), aes(y= T50.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  geom_hline(yintercept = 37.2, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  scale_x_discrete(limit=rev)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("July 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


grid.arrange(June2022_t50,July2022_t50, June2023_t50,July2023_t50,ncol=2)

# # # # # # # # # # # # #
# four month T95 plot----
# # # # # # # # # # # # #

#June 2022
June2022_t95 <- ggplot(tcrit%>%
                     filter(year==2022,month==6), aes(y= T95.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  geom_hline(yintercept = 38.3, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 80)+
  ggtitle("June 2022")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

July2022_t95 <- ggplot(tcrit%>%
                     filter(year==2022,month==7), aes(y= T95.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  geom_hline(yintercept = 38.9, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 80)+
  ggtitle("July 2022")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

June2023_t95 <- ggplot(tcrit%>%
                     filter(year==2023,month==6), aes(y= T95.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  geom_hline(yintercept = 38.3, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 80)+
  ggtitle("June 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

July2023_t95 <- ggplot(tcrit%>%
                     filter(year==2023,month==7), aes(y= T95.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  geom_hline(yintercept = 37.2, color= "blue")+ #high temp for the month
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp since 1980
  scale_x_discrete(limit=rev)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 80)+
  ggtitle("July 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

grid.arrange(June2022_t95,July2022_t95, June2023_t95,July2023_t95,ncol=2)


