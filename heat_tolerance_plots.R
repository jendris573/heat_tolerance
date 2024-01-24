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
outputs <- read_excel("data/crit_values_final.xlsx")
leaf_max_temp <- read_excel("data/leaf_temperatures.xlsx", sheet =2)

#filter just TN data
outputs<-outputs[which(outputs$state=="TN"),]

#create column for julian date
outputs$julian_date <- yday(outputs$date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$date))

#add column for image location - needed for plotting leaf image
leaf_max_temp$location<-'leaf_image2.jpg'#change this filename to whatever .jpg or .png - can only use jpg or png
#If using a different .jpg, make sure the file size is really small (<10 KB). Otherwise the plotting
#takes forever and you end up with a huge file size for the ggplot. Not sure why it does this.


# # # # # # # # # # # # # # # # # #
# Plots for each sample period ----
# # # # # # # # # # # # # # # # # #

### TCRIT for June/July 2022/2023 ###
#June 2022
June2022 <- ggplot(outputs%>%
                      filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 42.8, linetype = 2, color= "orange")+ #record june high temp
  geom_hline(yintercept = 38.3, linetype = 3, color= "blue")+ #highest temp June 2022
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.spacing.y = unit(0, "cm"),
        legend.position=c("RIGHT"))
June2022

July2022 <- ggplot(outputs%>%
                     filter(year==2022,month==7), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 43.3, linetype = 2, color= "orange")+ #record July high temp
  geom_hline(yintercept = 38.9, linetype = 3, color= "blue")+ #highest temp July 2022
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2022")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2022

June2023 <- ggplot(outputs%>%
                     filter(year==2023,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 42.8, linetype = 2, color= "orange")+ #record June high temp
  geom_hline(yintercept = 38.3, linetype = 3, color= "blue")+ #highest temp June 2023
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2023

July2023 <- ggplot(outputs%>%
                     filter(year==2023,month==7), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  scale_x_discrete(limit=rev)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 43.3, linetype = 2, color= "orange")+ #record July high temp
  geom_hline(yintercept = 37.2, linetype = 3, color= "blue")+ #highest temp July 2023
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2023

grid.arrange(June2022,July2022, June2023,July2023,ncol=2)

### T50 for June/July 2022/2023 ###
#June 2022
T50_June2022 <- ggplot(outputs%>%
                     filter(year==2022,month==6), aes(y= T50.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 42.8, linetype = 2, color= "orange")+ #record june high temp
  geom_hline(yintercept = 38.3, linetype = 3, color= "blue")+ #highest temp June 2022
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("June 2022")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.spacing.y = unit(0, "cm"),
        legend.position=c("RIGHT"))
T50_June2022

T50_July2022 <- ggplot(outputs%>%
                     filter(year==2022,month==7), aes(y= T50.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 43.3, linetype = 2, color= "orange")+ #record July high temp
  geom_hline(yintercept = 38.9, linetype = 3, color= "blue")+ #highest temp July 2022
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("July 2022")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
T50_July2022

T50_June2023 <- ggplot(outputs%>%
                     filter(year==2023,month==6), aes(y= T50.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 42.8, linetype = 2, color= "orange")+ #record June high temp
  geom_hline(yintercept = 38.3, linetype = 3, color= "blue")+ #highest temp June 2023
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("June 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
T50_June2023

T50_July2023 <- ggplot(outputs%>%
                     filter(year==2023,month==7), aes(y= T50.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  geom_errorbar(aes(ymax=T50.uci,ymin=T50.lci),width=0.5)+
  scale_x_discrete(limit=rev)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 43.3, linetype = 2, color= "orange")+ #record July high temp
  geom_hline(yintercept = 37.2, linetype = 3, color= "blue")+ #highest temp July 2023
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 60)+
  ggtitle("July 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
T50_July2023

grid.arrange(T50_June2022,T50_July2022, T50_June2023,T50_July2023,ncol=2)


### T95 for June/July 2022/2023 ###
#June 2022
T95_June2022 <- ggplot(outputs%>%
                     filter(year==2022,month==6), aes(y= T95.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 42.8, linetype = 2, color= "orange")+ #record june high temp
  geom_hline(yintercept = 38.3, linetype = 3, color= "blue")+ #highest temp June 2022
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(35, 80)+
  ggtitle("June 2022")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.spacing.y = unit(0, "cm"),
        legend.position=c("RIGHT"))
T95_June2022

T95_July2022 <- ggplot(outputs%>%
                     filter(year==2022,month==7), aes(y= T95.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 43.3, linetype = 2, color= "orange")+ #record July high temp
  geom_hline(yintercept = 38.9, linetype = 3, color= "blue")+ #highest temp July 2022
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(35, 80)+
  ggtitle("July 2022")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
T95_July2022

T95_June2023 <- ggplot(outputs%>%
                     filter(year==2023,month==6), aes(y= T95.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 42.8, linetype = 2, color= "orange")+ #record June high temp
  geom_hline(yintercept = 38.3, linetype = 3, color= "blue")+ #highest temp June 2023
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(35, 80)+
  ggtitle("June 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
T95_June2023

T95_July2023 <- ggplot(outputs%>%
                     filter(year==2023,month==7), aes(y= T95.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species,image=location),size=0.05)+
  geom_point()+
  geom_errorbar(aes(ymax=T95.uci,ymin=T95.lci),width=0.5)+
  scale_x_discrete(limit=rev)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 43.3, linetype = 2, color= "orange")+ #record July high temp
  geom_hline(yintercept = 37.2, linetype = 3, color= "blue")+ #highest temp July 2023
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(35, 80)+
  ggtitle("July 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
T95_July2023

grid.arrange(T95_June2022,T95_July2022,T95_June2023,T95_July2023,ncol=2)


sept23 <- ggplot(outputs%>%
                   filter(year==2023,month==9), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  scale_x_discrete(limit=rev)+
  geom_point(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=species), shape = 8, size=4)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  geom_hline(yintercept = 44.4, linetype = 2, color= "orange")+ #record Sept high temp
  geom_hline(yintercept = 33.9, linetype = 3, color= "blue")+ #highest temp Sept 2023
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(20, 55)+
  ggtitle("September 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
sept23


### Variation between 2022 and 2023

outputs$year <- as.factor(outputs$year)

june_variation <- outputs %>%
  filter(month==6)

june_var <- ggplot(june_variation, aes(y= Tcrit.mn, x= id, color=year)) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
june_var

july_variation <- outputs %>%
  filter(month==7)

july_var <- ggplot(july_variation, aes(y= Tcrit.mn, x= id, color=year)) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July")+
  theme_bw()+
  #theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
july_var

grid.arrange(june_var,july_var,ncol=2)





#####################################################################
#####################################################################
### Unused Plots below this line ### Unused Plots below this line ###
#####################################################################
#####################################################################

#Evan's version to plot LT50 by each sample period
full_plot <- ggplot(outputs, aes(y= Tcrit.mn, x= reorder(id, Tcrit.mn, decreasing = TRUE))) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
  # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  facet_wrap(~month+year)#since facet_wrap - the species will stay in the same order regardless of
#changing LT values. Therefore it is probably necessary to make a figure for each sample period
full_plot


##############################
###Individual Species Plots### 
##############################

#Acer saccharum
maple <- filter(outputs, id == 'Acer saccharum')

maple_plot <- ggplot(maple, aes(x=date, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Date")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

maple_plot

#Liriodendron tulipifera
poplar <- filter(outputs, id == 'Liriodendron tulipifera')

poplar_plot <- ggplot(poplar, aes(x=date, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Date")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

poplar_plot

#Fagus grandifolia
beech <- filter(outputs, id == 'Fagus grandifolia')

beech_plot <- ggplot(beech, aes(x=date, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Date")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

beech_plot

grid.arrange(maple_plot, poplar_plot, beech_plot,nrow=3)
