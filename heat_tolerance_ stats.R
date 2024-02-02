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
library(lme4)
library(nlme)

# # # # # # # # # # # # # # # # #
# Data entry and preparation ----
# # # # # # # # # # # # # # # # #

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

jj_boots <- read_excel("data/boot_1000.xlsx")

#separate ID column into separate columns
jj_boots <- separate(data = jj_boots, col = id, into = c("date", "state", "species"), sep = "\\.")

#create column for julian date
jj_boots$julian_date <- yday(jj_boots$date)

#change date column from character to date
jj_boots$Date2<-ymd(jj_boots$date)

#keep just 2022 and 2023
jj_boots<-jj_boots%>%
  filter(lubridate::year(Date2)%in%c(2022,2023))

#keep only June and July
jj_boots<-jj_boots%>%
  filter(lubridate::month(Date2)%in%c(6,7))

jj_boots$Date2 <- as.Date(jj_boots$Date2)

#create column for month
jj_boots$month <- as.factor(lubridate::month(jj_boots$Date2))

#create column for year
jj_boots$year <- as.factor(lubridate::year(jj_boots$Date2))

# # # # # # # # # # # # #
# Statistical Tests ----
# # # # # # # # # # # # #

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

# # # # # # # # # #
# Tcrit models ----
# # # # # # # # # #

jj_tcrit <- outputs %>%
  filter(month==6|month==7)

tcrit_models <- glm(Tcrit.mn ~ month * year, data= jj_tcrit, na.action="na.fail")

summary(tcrit_models)


tcrit_model2 <- lme(Tcrit.mn ~ as.factor(month) + as.factor(year) , random = ~ 1|species, data= jj_tcrit, na.action="na.fail")

summary(tcrit_model2)
anova(tcrit_model2)



##Boot 1000 models

jj_boots <- jj_boots[complete.cases(jj_boots[,5]),]

jj_boots_mod <- lme(tcrit ~ as.factor(month) + as.factor(year) , random = ~ 1|species, data= jj_tcrit %>% filter(species=="Acer saccharum"), na.action="na.fail")

summary(jj_boots_mod)

# # # # # # # # # # #
# Species models ----
# # # # # # # # # # #

#maple model
maple_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Acer saccharum"), na.action="na.fail")

summary(maple_mod)

#sugarberry model
sugarberry_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Celtis laevigata"), na.action="na.fail")

summary(sugarberry_mod)

#beech model
beech_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Fagus grandifolia"), na.action="na.fail")

summary(beech_mod)

#walnut model
walnut_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Juglans nigra"), na.action="na.fail")

summary(walnut_mod)

#sweetgum model
sweetgum_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Liquidambar styraciflua"), na.action="na.fail")

summary(sweetgum_mod)

#poplar model
poplar_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Liriodendron tulipifera"), na.action="na.fail")

summary(poplar_mod)

#hophornbeam model
hophornbeam_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Ostrya virginiana"), na.action="na.fail")

summary(hophornbeam_mod)

#cherry model
cherry_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Prunus serotina"), na.action="na.fail")

summary(cherry_mod)

#red oak model
redoak_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Quercus falcata"), na.action="na.fail")

summary(redoak_mod)

#chestnut oak model
chestnut_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Quercus montana"), na.action="na.fail")

summary(chestnut_mod)

#elm model
elm_mod <- lme(tcrit ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_boots %>% filter(species=="Ulmus rubra"), na.action="na.fail")

summary(elm_mod)









