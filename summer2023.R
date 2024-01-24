## R code to plot out the summer 2023 TSM
## written by Joe Endris

# Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggimage)
library(multcomp)
library(multcompView)
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

# # # # # # # # #
# Plots ----
# # # # # # # # #

maple <-   boots2023 %>%
  filter(species=="Acer saccharum")

maple_tcrit <- ggplot(
  aes(y=tcrit, x= month)+
    geom_line()+
    geom_smooth(method = lm)+
    ggtitle("Acer saccharum"))

maple_tcrit

