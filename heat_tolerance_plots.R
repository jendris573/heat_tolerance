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

outputs_species <- melt(outputs_species, id.vars = 'id', variable.name = 'Threshold')




full_plot <- ggplot(outputs_species, aes(x = Threshold, y=id)) +
  geom_point()+
  ylab("Species")+
  xlab("Critical Temperature")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

full_plot






