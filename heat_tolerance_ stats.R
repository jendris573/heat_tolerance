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

#rename id as species
colnames(outputs)[1] = "species"

#######################
###Statistical Tests###
#######################

##global models
tcrit_global_mod <- glm(Tcrit.mn ~ year * species * julian_date, data=outputs, na.action="na.fail")
dredge(tcrit_global_mod)

t50_global_mod <- glm(T50.mn ~ year * species * julian_date, data=outputs, na.action="na.fail")
dredge(t50_global_mod)

t95_global_mod <- glm(T95.mn ~ year * species * julian_date, data=outputs, na.action="na.fail")
dredge(t95_global_mod)

##best models
tcrit_mod <- glm(Tcrit.mn ~ year, data=outputs, na.action="na.fail")
summary(tcrit_mod)

t50_mod <- glm(T50.mn ~ species + year, data=outputs, na.action="na.fail")
summary(t50_mod)

t50_mod$species <- as.factor(t50_mod$species)
summary(glht(t50_mod, mcp(species= "Tukey")))

t95_mod <- glm(T95.mn ~ species + year, data=outputs, na.action="na.fail")
summary(t95_mod)

t95_mod$species = as.factor(t95_mod$species)
summary(glht(t95_mod, mcp(species= "Tukey")))




