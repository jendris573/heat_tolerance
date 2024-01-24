## R code to plot out the summer 2023 TSM
## written by Joe Endris

# Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggimage)
library(gridExtra)
library(lubridate)
library(readxl)

# # # # # # # # #
# Data prep ----
# # # # # # # # #

boots <- read_excel("data/boot_1000.xlsx")

#separate ID column into separate columns
boots <- separate(data = boots, col = id, into = c("date", "state", "species"), sep = "\\.")

#create column for julian date
boots$julian_date <- yday(boots$date)

#create column for month
boots <- mutate(boots, month=month(boots$date))

#create column for year
boots <- mutate(boots, year=year(boots$date))

#filter for only 2023 data
boots2023 <- boots %>%
  filter(year==2023)

#omit any blank spots in the tcrit column
boots2023 <- boots2023[complete.cases(boots2023[,5]),]

#calculate mean tcrit
boots2023 <- boots2023%>%
  group_by(species, julian_date) %>%
  dplyr::summarise(mean_tcrit=mean(tcrit))

# # # # # # #
# Plots ----
# # # # # # #

#1
maple_plot<-ggplot(data=subset(boots2023, species=="Acer saccharum"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Acer saccharum")

maple_plot

#2
sugarberry_plot<-ggplot(data=subset(boots2023, species=="Celtis laevigata"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Celtis laevigata")

sugarberry_plot

#3
beech_plot<-ggplot(data=subset(boots2023, species=="Fagus grandifolia"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Fagus grandifolia")

beech_plot

#4
walnut_plot<-ggplot(data=subset(boots2023, species=="Juglans nigra"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Juglans nigra")

walnut_plot

#5
sweetgum_plot<-ggplot(data=subset(boots2023, species=="Liquidambar styraciflua"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Liquidambar styraciflua")

sweetgum_plot

#6
redoak_plot<-ggplot(data=subset(boots2023, species=="Quercus falcata"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Quercus falcata")

redoak_plot

#7
chestnutoak_plot<-ggplot(data=subset(boots2023, species=="Quercus montana"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Quercus montana")

chestnutoak_plot

#8
cherry_plot<-ggplot(data=subset(boots2023, species=="Prunus serotina"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Prunus serotina")

cherry_plot

#9
poplar_plot<-ggplot(data=subset(boots2023, species=="Liriodendron tulipifera"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Liriodendron tulipifera")

poplar_plot

#10
hophornbeam_plot<-ggplot(data=subset(boots2023, species=="Ostrya virginiana"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Ostrya virginiana")

hophornbeam_plot

#11
elm_plot<-ggplot(data=subset(boots2023, species=="Ulmus rubra"), aes(x = julian_date, y=mean_tcrit)) +
  geom_line()+
  geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Ulmus rubra")

elm_plot


grid.arrange(beech_plot, maple_plot, chestnutoak_plot, redoak_plot, poplar_plot,
             sweetgum_plot, walnut_plot, cherry_plot, sugarberry_plot, hophornbeam_plot, 
             elm_plot, nrow=4)













