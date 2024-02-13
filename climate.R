### Code to provide climate numbers for site description for the APSU forest
### Written by Joe Endris

# # # # # # # # #
# Libraries ----
# # # # # # # # #

library(readxl)
library(writexl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)
library(multcomp)
library(ggplot2)
library(gridExtra)

# # # # # # # # # # # #
## Data Preparation ----
# # # # # # # # # # # #

#Load NOAA Climate Data Online data
tenn_clim<-read_excel("data/tenn1980.xlsx")

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

#omit NA in TMAX recordings 
tenn_TMAX<-tenn_clim[complete.cases(tenn_clim[,5]),]
#omit NA in precipitation recordings 
#tenn_precip<-tenn_clim[complete.cases(tenn_clim[,4]),]
#omit NA in TMIN recordings 
#tenn_TMIN<-tenn_clim[complete.cases(tenn_clim[,6]),]

# # # # # # # # # # # # # #
# Climate data points ----
# # # # # # # # # # # # # #

#determine annual precipitation values
precip <- tenn_precip %>%
  group_by(year) %>%
  dplyr::summarise(annual_precip = sum(PRCP))

#average annual TMAX
TMAX <- tenn_TMAX %>%
  group_by(year) %>%
  dplyr::summarise(annual_TMAX = mean(TMAX))

#average annual TMIN
TMIN <- tenn_TMIN %>%
  group_by(year) %>%
  dplyr::summarise(annual_TMIN = mean(TMIN))

#create one data frame with all the data
climate <- cbind(precip, TMAX$annual_TMAX, TMIN$annual_TMIN) %>%
  rename("TMAX" = "TMAX$annual_TMAX",
         "TMIN" = "TMIN$annual_TMIN")

#calculate the mean high temperature
mean_TMAX <-   climate %>%
  dplyr::summarise(annual_TMAX = mean(TMAX))
#calculate the mean low temperature
mean_TMIN <-   climate %>%
  dplyr::summarise(annual_TMIN = mean(TMIN))
#calculate the mean precipitation
mean_precip <-   climate %>%
  dplyr::summarise(mean_precip = mean(annual_precip))

# # # # # # # # # # # # # # # # # # # # # #
# Calculate Absolute High Temperatures ----
# # # # # # # # # # # # # # # # # # # # # #

#Determine absolute hottest day by year
tenn_clim$DATE <- as.Date(tenn_clim$DATE)
class(tenn_clim$DATE)

#record high temperature since 1900
record_TMAX <- tenn_clim %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for May
may_TMAX <- tenn_clim %>%
  filter(month==5) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June
june_TMAX <- tenn_clim %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for July
july_TMAX <- tenn_clim %>%
  filter(month==7) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for August
aug_TMAX <- tenn_clim %>%
  filter(month==8) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for September
sep_TMAX <- tenn_clim %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June 2022
june2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for May 2023
may2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==5) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June 2023
june2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for July 2022
july2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==7) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

# # # # # # # # # # # # # # # # #
# Climate statistical models ----
# # # # # # # # # # # # # # # # #

heat_season <- tenn1980 %>%
  filter(julian_date>120) %>%
  filter(julian_date<275)

heat_season_mod <- lm(TMAX ~ year, data=heat_season)

summary(high_temp_mod)

#filter 1980 data for may only
may_mean_tmax <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>120) %>%
  filter(julian_date<152) %>%
  dplyr::summarise(temp=mean(TMAX))

may_model <- glm(temp ~ julian_date + year, data = may_mean_tmax, na.action="na.fail")
summary(may_model)

#filter 1980 data for June only
june_mean_tmax <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>151) %>%
  filter(julian_date<182) %>%
  dplyr::summarise(temp=mean(TMAX))

june_model <- glm(temp ~ julian_date + year, data = june_mean_tmax, na.action="na.fail")
summary(june_model)

#filter 1980 data for July only
july_mean_tmax <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>181) %>%
  filter(julian_date<213) %>%
  dplyr::summarise(temp=mean(TMAX))

july_model <- glm(temp ~ julian_date + year, data = july_mean_tmax, na.action="na.fail")
summary(july_model)

#filter 1980 data for August only
aug_mean_tmax <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>212) %>%
  filter(julian_date<244) %>%
  dplyr::summarise(temp=mean(TMAX))

aug_model <- glm(temp ~ julian_date + year, data = aug_mean_tmax, na.action="na.fail")
summary(aug_model)

#filter 1980 data for Sept only
sept_mean_tmax <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>243) %>%
  filter(julian_date<274) %>%
  dplyr::summarise(temp=mean(TMAX))

sept_model <- glm(temp ~ julian_date + year, data = sept_mean_tmax, na.action="na.fail")
summary(sept_model)

annual_model <- glm(TMAX ~ julian_date + year, data = tenn1980 )
summary(annual_model)


# # # # # # # # # # # # # # # # # # # # 
# Look for NAs in temperature data ----
# # # # # # # # # # # # # # # # # # # #

#determine number of NA observations in TMAX for 2022 and 2023
nas <- read.csv("data/Tennessee_climate.csv")

nas <- mutate(nas, year=year(nas$DATE))

nas_2022 <- nas %>%
  filter(year == 2022)

sum(is.na(nas_2022$TMAX))

nas_2023 <- nas %>%
  filter(year == 2023)

sum(is.na(nas_2023$TMAX))

nas_1980 <- nas %>%
  filter(year>1979)

sum(is.na(nas_1980$TMAX))

nas_2010 <- nas %>%
  filter(year == 2010)
sum(is.na(nas_2010$TMAX))
