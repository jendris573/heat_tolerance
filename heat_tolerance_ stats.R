###Heat Tolerance Statistical Tests
###Written by Joe Endris

#Libraries
library(dplyr)
library(tidyr)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(xlsx)
library(gridExtra)
library(car)
library(stringr)
library(MuMIn)

##################################
### Data entry and preparation ###
##################################

outputs <- read_excel("data/crit_values_final.xlsx")

#create column for julian date
outputs$julian_date <- yday(outputs$date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$date))

#rename id as species for merging
colnames(outputs)[1] = "species"

#read in leaf temp data
leaf_temp_data <- read_excel("data/leaf_temperatures.xlsx", sheet =2)

#merge the two dataframes
leaf_temps <- merge(outputs, leaf_temp_data, by=c("species", "year"))

#######################
###Statistical Tests###
#######################

##global models
tcrit_global_mod <- glm(Tcrit.mn ~ year * julian_date, data=outputs, na.action="na.fail")
dredge(tcrit_global_mod)

t50_global_mod <- glm(T50.mn ~ year * julian_date, data=outputs, na.action="na.fail")
dredge(t50_global_mod)

t95_global_mod <- glm(T95.mn ~ year * julian_date, data=outputs, na.action="na.fail")
dredge(t95_global_mod)

##best models
tcrit_mod <- glm(Tcrit.mn ~ year, data=outputs, na.action="na.fail")
summary(tcrit_mod)

t50_mod <- glm(T50.mn ~ year, data=outputs, na.action="na.fail")
summary(t50_mod)

t95_mod <- glm(T95.mn ~ year, data=outputs, na.action="na.fail")
summary(t95_mod)


##leaf temperature vs Tcrit model
leaf_temp_tcrit_mod <- glm(Tcrit.mn ~ leaf_temp, data=leaf_temps, na.action="na.fail")

summary(leaf_temp_tcrit_mod)

leaf_temp_tcrit_mod_b <- glm(Tcrit.mn ~ leaf_temp * species, data=leaf_temps, na.action="na.fail")

dredge(leaf_temp_tcrit_mod_b)


