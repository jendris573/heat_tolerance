###Heating Tolerance Plots###
###Code written by Joe Endris###

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)
library(reshape2)
library(gridGraphics)

##################################
### Data entry and preparation ###
##################################

outputs <-read_excel ("/Users/Joe/Documents/College/01- Data/crit_values_TN.xlsx")

#create column for julian date
outputs$julian_date <- yday(outputs$date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$date))

#######################
###Full species plot###
#######################

outputs_species <- outputs%>%
  group_by(id)%>%
  dplyr::summarise(across(Tcrit.lci:T95.uci, list(mean=~mean(.))))

str(outputs_species)

as.factor(outputs_species$id)

full_plot <- ggplot(outputs_species, aes(x= outputs_species$Tcrit.mn_mean, y= reorder(id, Tcrit.mn_mean, decreasing = TRUE, color = "red"))) +
  geom_point()+
  geom_errorbar(data=subset(outputs_species, group_by("id")), aes(ymax=Tcrit.mn_mean+Tcrit.uci_mean,ymin=Tcrit.mn_mean-Tcrit.lci_mean, color = "red"))+
 # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
 # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Species")+
  xlab("Critical Temperature")+
  xlim(40, 65)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

full_plot






