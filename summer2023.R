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

boots2023 <- read_excel("data/boot_1000.xlsx")

#separate ID column into separate columns
boots2023 <- separate(data = boots2023, col = id, into = c("date", "state", "species"), sep = "\\.")

#create column for julian date
boots2023$julian_date <- yday(boots2023$date)

#create column for month
boots2023 <- mutate(boots2023, month=month(boots2023$date))

#create column for year
boots2023 <- mutate(boots2023, year=year(boots2023$date))

#filter for only 2023 data
boots2023 <- boots2023 %>%
  filter(year==2023)

#omit any blank spots in the tcrit column
boots2023 <- boots2023[complete.cases(boots2023[,5]),]

#calculate mean tcrit
boots2023 <- boots2023%>%
  group_by(species, julian_date) %>%
  dplyr::summarise(mean_tcrit=mean(tcrit))

#Load NOAA Climate Data Online data
tenn1980<-read.csv("data/Tennessee_climate.csv")

#create column for year
tenn1980 <- mutate(tenn1980, year=year(tenn1980$DATE))

#create column for month
tenn1980 <- mutate(tenn1980, month=month(tenn1980$DATE))

## create column for julian date##
tenn1980$julian_date <- yday(tenn1980$DATE)

#omit NA in TMAX/TMIN recordings 
tenn1980<-tenn1980[complete.cases(tenn1980[,9]),]

#filter for 1980-present
tenn1980 <- tenn1980 %>%
  filter(year>1979)

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

# # # # # # # # # #
# Core 3 Plots ----
# # # # # # # # # #

core_three <- boots2023 %>%
  filter(species=="Acer saccharum"|species== "Fagus grandifolia"| species=="Liriodendron tulipifera")

core_plot<-ggplot(data=core_three, aes(x = julian_date, y=mean_tcrit, color=species)) +
  geom_line()+
  geom_hline(yintercept = 43.5, linetype = 2, color= "red")+ #max leaf temp - maple
  geom_hline(yintercept = 41.0, linetype = 2, color= "green")+ #max leaf temp - beech
  geom_hline(yintercept = 38.7, linetype = 2, color= "blue")+ #max leaf temp - poplar
  #geom_smooth(method = loess)+
  labs(x="Julian Date", y="Tcrit")+
  theme_bw()+
  ggtitle("Core 3 Species in 2023")

core_plot




# # # # # # # # # # # #
# Core Three Stats ----
# # # # # # # # # # # #

core_3_mod <- glm(mean_tcrit ~ species * julian_date, data=core_three, na.action="na.fail")

summary(core_3_mod)






