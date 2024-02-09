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
library(ggplot2)

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

####Can we remove these global models since we aren't using them#############
# ##global models
# tcrit_global_mod <- glm(Tcrit.mn ~ year * julian_date, data=outputs, na.action="na.fail")
# dredge(tcrit_global_mod)
# 
# t50_global_mod <- glm(T50.mn ~ year * julian_date, data=outputs, na.action="na.fail")
# dredge(t50_global_mod)
# 
# t95_global_mod <- glm(T95.mn ~ year * julian_date, data=outputs, na.action="na.fail")
# dredge(t95_global_mod)
# 
# ##best models
# tcrit_mod <- glm(Tcrit.mn ~ year, data=outputs, na.action="na.fail")
# summary(tcrit_mod)
# 
# t50_mod <- glm(T50.mn ~ year, data=outputs, na.action="na.fail")
# summary(t50_mod)
# 
# t95_mod <- glm(T95.mn ~ year, data=outputs, na.action="na.fail")
# summary(t95_mod)


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

#model without species
tcrit_models <- glm(Tcrit.mn ~ month * year, data= jj_tcrit, na.action="na.fail")

summary(tcrit_models)

#build the full model and remove terms as needed
tcrit_model2 <- lme(Tcrit.mn ~ as.factor(month) * as.factor(year) , random = ~ 1|species, data= jj_tcrit, na.action="na.fail")

summary(tcrit_model2)#interaction not significant, remove and view performance
#final best model that excludes the interaction
tcrit_model3 <- lme(Tcrit.mn ~ as.factor(month) + as.factor(year) , random = ~ 1|species, data= jj_tcrit, na.action="na.fail")
summary(tcrit_model3)
anova(tcrit_model3)

##Boot 1000 models
##We provide this 1000 bootstrap approach just to have a look at the model with all data
#however, there is so much data entering that basically everything is significant

jj_boots <- jj_boots[complete.cases(jj_boots[,5]),]

jj_boots_mod <- lme(tcrit ~ as.factor(month) + as.factor(year) , random = ~ 1|species, data= jj_boots, na.action="na.fail")

summary(jj_boots_mod)

# # # # # # # # # # #
# Species models ----
# # # # # # # # # # #


#maple model
maple_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Acer saccharum"), na.action="na.fail")

summary(maple_mod)

#sugarberry model
sugarberry_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Celtis laevigata"), na.action="na.fail")

summary(sugarberry_mod)

#beech model
beech_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Fagus grandifolia"), na.action="na.fail")

summary(beech_mod)

#walnut model
walnut_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Juglans nigra"), na.action="na.fail")

summary(walnut_mod)

#sweetgum model
sweetgum_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Liquidambar styraciflua"), na.action="na.fail")

summary(sweetgum_mod)

#poplar model
poplar_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Liriodendron tulipifera"), na.action="na.fail")

summary(poplar_mod)

#hophornbeam model
hophornbeam_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Ostrya virginiana"), na.action="na.fail")

summary(hophornbeam_mod)

#cherry model
cherry_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Prunus serotina"), na.action="na.fail")

summary(cherry_mod)

#red oak model
redoak_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Quercus falcata"), na.action="na.fail")

summary(redoak_mod)

#chestnut oak model
chestnut_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Quercus montana"), na.action="na.fail")

summary(chestnut_mod)

#elm model
elm_mod <- glm(tcrit ~ as.factor(month) * as.factor(year), data= jj_boots %>% filter(species=="Ulmus rubra"), na.action="na.fail")

summary(elm_mod)


# # # # # # # # # # # # # #
# Leaf Temp TSM models ----
# # # # # # # # # # # # # #

#import data of mean thermal safety margin values for each species and sample
#note the _safety columns are the thermal safety margin between tcrit, t50 or t95 versus leaf temperature
#_safety_air columns are the thermal safety margin between tcrit, t50 or t95 versus air temperature
therm <- read.csv("data/mean_thermal_safety_leaf_air.csv")
#create ID column
therm$ID<-paste(therm$Species,therm$year,therm$month,sep=".")
therm2<-therm%>%
  group_by(ID)%>%
  pivot_longer(cols=c(tcrit_safety,tcrit_safety_air))
#leaf to air thermal safety margins
#this will be a simple paired t-test as we are comparing thermals safety margin or tcrit v leaf and tcrit v air
t.test(therm2$value~therm2$name,paired=TRUE,alternative="two.sided")#very small p-value means there is a difference
#simple plot to visualize the difference - thermal safety margin compared to air is higher than compared to leaf
ggplot(therm2,aes(x=name,y=value))+
  geom_boxplot()

#second set of models looking at how thermal safety margins may change over months and year

#start with only thermal safety margin based on leaf temp
leaf_tsm_model <- lme(tcrit_safety ~ as.factor(month) * as.factor(year) , random = ~ 1|Species, data= therm, na.action="na.fail")
summary(leaf_tsm_model)#the interaction is not significant so drop it
leaf_tsm_model <- lme(tcrit_safety ~ as.factor(month) + as.factor(year) , random = ~ 1|Species, data= therm, na.action="na.fail")
summary(leaf_tsm_model)#best model very similar to tcrit model
#safety margin increase from June to July - this is counterintuitive since temps are hotter in July. I guess trees
#are raising tcrit faster than temp is increasing
#safety margin decreases from 2022 to 2023. Might be related to SPEI index and that drought stressed trees have higher tcrit

##build second model but with thermal safety margin compared to air temperature
#I wonder if thise model is needed since we are arguing that air temperature is not a good measure of real danger
leaf_tsm_model <- lme(tcrit_safety_air ~ as.factor(month) * as.factor(year) , random = ~ 1|Species, data= therm, na.action="na.fail")
summary(leaf_tsm_model)#the interaction is not significant so drop it, but year is also potentially not significant
leaf_tsm_model <- lme(tcrit_safety ~ as.factor(month) + as.factor(year) , random = ~ 1|Species, data= therm, na.action="na.fail")
summary(leaf_tsm_model)#once dropping the interaction year becomes significant. Again this model is similar to the tcrit only model




