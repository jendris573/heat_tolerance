#test script to compare critical temps to threshold temperature values
#climate data is coming from NOAA and thresholds determined in a separate scripts


#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(xlsx)
library(gridExtra)
library(car)
library(stringr)

#################
### Data prep ###
#################

#read in data
heating_data<-read_excel("data/boot_1000.xlsx")

#clean and organize data
#separate the ID column
#Separate back out the date from the ID column
heating_data$date<-substr(heating_data$id, 1, 10)

#Separate back out the state from the ID column
heating_data$state<-substr(heating_data$id, 12, 13)

#remove the date from the ID column
heating_data$id<-str_sub(heating_data$id, 15, )

#convert these new columns back to numeric
heating_data$date<-as.Date(heating_data$date)

#separate date column into day/month/year
heating_data<-heating_data %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))

#load the mean and CI critical values for these same species so you can compare
crits<-read_excel("data/crit_values_final.xlsx")

#########################
### Predefined Values ###
#########################

#absolute record high temp for Clarksville since 1900
thresh<-44.4
#absolute highest temp recorded in June since 1900
juneTMAX <- 42.8
#absolute highest temp recorded for July since 1900
julyTMAX <- 43.3
#absolute highest temp recorded for August since 1900
augTMAX <- 42.8
#absolute highest temp recorded in June 2022
june2022TMAX <- 38.3
#absolute highest temp recorded in June 2023
june2023TMAX <- 38.3
#absolute highest temp recorded in July 2022
july2022TMAX <- 38.9
#absolute highest temp recorded in July 2023
july2023TMAX <- 37.2
#absolute highest temp recorded in August 2023
aug2023TMAX <- 37.8
#absolute highest temp recorded in September 2023
sep2023TMAX <- 33.9

species <- c("Acer saccharum", "Celtis laevigata", "Fagus grandifolia", "Juglans nigra", "Liquidambar styraciflua", "Liriodendron tulipifera", "Ostrya virginiana", "Prunus serotina", "Quercus falcata", "Quercus montana", "Ulmus americana")
leaf2022 <- c(38.8, 38.9, 45.3, 37.6, 36.5, 44.5, 40.6, 40.2, 45.7, 41.9, 45.9)
leaf2023 <- c(43.5, 38.7, 41.0, 40.8, 38.7, 44.8, 40.7, 39.5, 43.5, 45.3, 42.1)

############################################################################
### test significance of bootstrapped outputs against specific thresholds ###
############################################################################

##Compare June 2022 thresholds to air temperatures

#create an empty dataframe
heat_comp <- as.data.frame(matrix(NA, nrow=11, ncol=10))

#define month and year
m1 <- 6
y1 <- 2022

#Loop to compare June 2022 versus air temperature thresholds
for (i in 1:length(species)) {
  test<-heating_data%>%
  filter(id==species[i])%>%
  filter(month==m1)%>%
  filter(year==y1) %>%
  filter(!is.na(tcrit))
heat_comp[i,1] = species [i]
heat_comp[i,2] = round(1-mean(test$tcrit>=thresh), 4)
heat_comp[i,3] = round(1-mean(test$tcrit>=juneTMAX), 4)
heat_comp[i,4] = round(1-mean(test$tcrit>=june2022TMAX), 4)

test2<-heating_data%>%
  filter(id==species[i])%>%
  filter(month==m1)%>%
  filter(year==y1) %>%
  filter(!is.na(T50))
heat_comp[i,5] = round(1-mean(test2$T50>=thresh), 4)
heat_comp[i,6] = round(1-mean(test2$T50>=juneTMAX), 4)
heat_comp[i,7] = round(1-mean(test2$T50>=june2022TMAX), 4)

test3<-heating_data%>%
  filter(id==species[i])%>%
  filter(month==m1)%>%
  filter(year==y1) %>%
  filter(!is.na(T95))
heat_comp[i,8] = round(1-mean(test3$T95>=thresh), 4)
heat_comp[i,9] = round(1-mean(test3$T95>=juneTMAX), 4)
heat_comp[i,10] = round(1-mean(test3$T95>=june2022TMAX), 4)
}

colnames(heat_comp)<- c("species", "absolute high (tcrit)", "June absolute (tcrit)", "June 2022 (tcrit)",
                     "absolute high (T50)", "June absolute (T50)", "June 2022 (T50)",
                     "absolute high (T95)", "June absolute (T95)", "June 2022 (T95)")

write.xlsx(heat_comp,"data/threshold_p.xlsx", sheetName = "June_2022", row.names = FALSE)

##Compare June 2023 thresholds to air temperatures

#create an empty dataframe for June 2023
heat_comp <- as.data.frame(matrix(NA, nrow=11, ncol=10))

#define month and year
m1 <- 6
y1 <- 2023

#Loop to compare June 2023 versus air temperature thresholds
for (i in 1:length(species)) {
  test<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(tcrit))
  heat_comp[i,1] = species [i]
  heat_comp[i,2] = round(1-mean(test$tcrit>=thresh), 4)
  heat_comp[i,3] = round(1-mean(test$tcrit>=juneTMAX), 4)
  heat_comp[i,4] = round(1-mean(test$tcrit>=june2022TMAX), 4)
  
  test2<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T50))
  heat_comp[i,5] = round(1-mean(test2$T50>=thresh), 4)
  heat_comp[i,6] = round(1-mean(test2$T50>=juneTMAX), 4)
  heat_comp[i,7] = round(1-mean(test2$T50>=june2022TMAX), 4)
  
  test3<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T95))
  heat_comp[i,8] = round(1-mean(test3$T95>=thresh), 4)
  heat_comp[i,9] = round(1-mean(test3$T95>=juneTMAX), 4)
  heat_comp[i,10] = round(1-mean(test3$T95>=june2022TMAX), 4)
}

colnames(heat_comp)<- c("species", "absolute high (tcrit)", "June absolute (tcrit)", "June 2023 (tcrit)",
                        "absolute high (T50)", "June absolute (T50)", "June 2023 (T50)",
                        "absolute high (T95)", "June absolute (T95)", "June 2023 (T95)")
  
write.xlsx(heat_comp,"data/threshold_p.xlsx", sheetName = "June_2023", append = TRUE, row.names = FALSE)

##Compare July 2022 thresholds to air temperatures and leaf temperatures

#create an empty dataframe for July 2022
heat_comp <- as.data.frame(matrix(NA, nrow=11, ncol=13))

#define month and year
m1 <- 7
y1 <- 2022

#Loop to compare July 2022 versus air temperature thresholds
for (i in 1:length(species)) {
  test<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(tcrit))
  heat_comp[i,1] = species [i]
  heat_comp[i,2] = round(1-mean(test$tcrit>=thresh), 4)
  heat_comp[i,3] = round(1-mean(test$tcrit>=juneTMAX), 4)
  heat_comp[i,4] = round(1-mean(test$tcrit>=june2022TMAX), 4)
  
  test2<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T50))
  heat_comp[i,5] = round(1-mean(test2$T50>=thresh), 4)
  heat_comp[i,6] = round(1-mean(test2$T50>=juneTMAX), 4)
  heat_comp[i,7] = round(1-mean(test2$T50>=june2022TMAX), 4)
  
  test3<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T95))
  heat_comp[i,8] = round(1-mean(test3$T95>=thresh), 4)
  heat_comp[i,9] = round(1-mean(test3$T95>=juneTMAX), 4)
  heat_comp[i,10] = round(1-mean(test3$T95>=june2022TMAX), 4)
  heat_comp[i,11] = round(1-mean(test$tcrit>=leaf2022[i]), 4)
  heat_comp[i,12] = round(1-mean(test$T50>=leaf2022[i]), 4)
  heat_comp[i,13] = round(1-mean(test$T95>=leaf2022[i]), 4)
}

colnames(heat_comp)<- c("species", "absolute high (tcrit)", "July absolute (tcrit)", "July 2022 (tcrit)",
                        "absolute high (T50)", "July absolute (T50)", "July 2022 (T50)",
                        "absolute high (T95)", "July absolute (T95)", "July 2022 (T95)", 
                        "Leaf Temp (tcrit)", "Leaf Temp (T50)", "Leaf Temp (T95)")
write.xlsx(heat_comp,"data/threshold_p.xlsx", sheetName = "July_2022", append = TRUE, row.names = FALSE)

##Compare July 2023 thresholds to air temperatures and leaf temperatures

#create an empty dataframe for July 2023
heat_comp <- as.data.frame(matrix(NA, nrow=11, ncol=13))

#define month and year
m1 <- 7
y1 <- 2023

#Loop to compare July 2023 versus air temperature thresholds
for (i in 1:length(species)) {
  test<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(tcrit))
  heat_comp[i,1] = species [i]
  heat_comp[i,2] = round(1-mean(test$tcrit>=thresh), 4)
  heat_comp[i,3] = round(1-mean(test$tcrit>=juneTMAX), 4)
  heat_comp[i,4] = round(1-mean(test$tcrit>=june2022TMAX), 4)
  
  test2<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T50))
  heat_comp[i,5] = round(1-mean(test2$T50>=thresh), 4)
  heat_comp[i,6] = round(1-mean(test2$T50>=juneTMAX), 4)
  heat_comp[i,7] = round(1-mean(test2$T50>=june2022TMAX), 4)
  
  test3<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T95))
  heat_comp[i,8] = round(1-mean(test3$T95>=thresh), 4)
  heat_comp[i,9] = round(1-mean(test3$T95>=juneTMAX), 4)
  heat_comp[i,10] = round(1-mean(test3$T95>=june2022TMAX), 4)
  heat_comp[i,11] = round(1-mean(test$tcrit>=leaf2023[i]), 4)
  heat_comp[i,12] = round(1-mean(test$T50>=leaf2023[i]), 4)
  heat_comp[i,13] = round(1-mean(test$T95>=leaf2023[i]), 4)
}

colnames(heat_comp)<- c("species", "absolute high (tcrit)", "July absolute (tcrit)", "July 2023 (tcrit)", 
                        "absolute high (T50)", "July absolute (T50)", "July 2023 (T50)",
                        "absolute high (T95)", "July absolute (T95)", "July 2023 (T95)", 
                        "Leaf Temp (tcrit)", "Leaf Temp (T50)", "Leaf Temp (T95)")
write.xlsx(heat_comp,"data/threshold_p.xlsx", sheetName = "July_2023", append = TRUE, row.names = FALSE)

##Compare September 2023 thresholds to air temperatures

#create an empty dataframe for September 2023
heat_comp <- as.data.frame(matrix(NA, nrow=11, ncol=10))

#define month and year
m1 <- 9
y1 <- 2023

#Loop to compare September 2023 versus air temperature thresholds
for (i in 1:length(species)) {
  test<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(tcrit))
  heat_comp[i,1] = species [i]
  heat_comp[i,2] = round(1-mean(test$tcrit>=thresh), 4)
  heat_comp[i,3] = round(1-mean(test$tcrit>=juneTMAX), 4)
  heat_comp[i,4] = round(1-mean(test$tcrit>=june2022TMAX), 4)
  
  test2<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T50))
  heat_comp[i,5] = round(1-mean(test2$T50>=thresh), 4)
  heat_comp[i,6] = round(1-mean(test2$T50>=juneTMAX), 4)
  heat_comp[i,7] = round(1-mean(test2$T50>=june2022TMAX), 4)
  
  test3<-heating_data%>%
    filter(id==species[i])%>%
    filter(month==m1)%>%
    filter(year==y1) %>%
    filter(!is.na(T95))
  heat_comp[i,8] = round(1-mean(test3$T95>=thresh), 4)
  heat_comp[i,9] = round(1-mean(test3$T95>=juneTMAX), 4)
  heat_comp[i,10] = round(1-mean(test3$T95>=june2022TMAX), 4)
}

colnames(heat_comp)<- c("species", "absolute high (tcrit)", "September absolute (tcrit)", "September 2023 (tcrit)",
                        "absolute high (T50)", "September absolute (T50)", "September 2023 (T50)",
                        "absolute high (T95)", "September absolute (T95)", "September 2023 (T95)")

write.xlsx(heat_comp,"data/threshold_p.xlsx", sheetName = "Sept_2023", append = TRUE, row.names = FALSE)









