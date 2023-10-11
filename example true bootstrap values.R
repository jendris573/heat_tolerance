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
#read in data
#read in data
heating_data<-read_excel("data/boot_1000.xlsx")

#Step1: estimate true values
#will focus on Tulpop and Ostvir in July 2022 as an example
heating_data2<-heating_data[c(1201:1300,1701:1800),]
quantile(heating_data2$tcrit[heating_data2$id=="2022-07-26.TN.Celtis laevigata"],c(0.025,0.5,0.975),na.rm=T)
quantile(heating_data2$tcrit[heating_data2$id!="2022-07-26.TN.Celtis laevigata"],c(0.025,0.5,0.975),na.rm=T)
test.stat.1<-abs(median(heating_data2$tcrit[heating_data2$id=="2022-07-26.TN.Celtis laevigata"])-
                   median(heating_data2$tcrit[heating_data2$id!="2022-07-26.TN.Celtis laevigata"]))
#true difference 4.2801
test.stat.1

#permutation test
set.seed(1981)
#the number of observations to sample
n<-length(heating_data2$id)
#number of permutation datasets
P<-10000
#The variable to shuffle - shuffle the tcrit
variable<-heating_data2$tcrit

#initialize a matrix to store permutation data
PermSamples<-matrix(0,nrow=n,ncol=P)

#use loop to generate permutations
for(i in 1:P){
  PermSamples[,i]<-sample(variable,size=n,replace=FALSE)
}
#look at first 5 columns
PermSamples[,1:5]
#need to add back in labels here

#make a loop to calculate the test-stats for each sample
Perm.test.stat1<-rep(0,P)
#loop through and calculate test stat
for(i in 1:P){
Perm.test.stat1[i]<-abs(median(PermSamples[heating_data2$id=="2022-07-26.TN.Celtis laevigata",i])-
                          median(PermSamples[heating_data2$id!="2022-07-26.TN.Celtis laevigata",i]))
}
max(Perm.test.stat1)#since not a single value was greater than the true difference, the p-value =0
#May need to use the actual bootstrap estimates, not the mean of the bootstrap estimates
mean(Perm.test.stat1>=test.stat.1)
#try again with two species not that far apart in terms of values
###############################################
##################################################
#Something is still not right as p-values always end up = 0
#Step1: estimate true values
#will focus on Acer and Fagus in June 2022 as an example
heating_data2<-heating_data[c(1:100,201:300),]

test.stat.1<-abs(mean(heating_data2$tcrit[heating_data2$id=="2022-06-21.TN.Acer saccharum"])-
                   mean(heating_data2$tcrit[heating_data2$id!="2022-06-21.TN.Acer saccharum"]))
#true difference 3.37
test.stat.1

#permutation test
set.seed(1981)
#the number of observations to sample
n<-length(heating_data2$id)
#number of permutation datasets
P<-100000
#The variable to shuffle - shuffle the tcrit
variable<-heating_data2$tcrit

#initialize a matrix to store permutation data
PermSamples<-matrix(0,nrow=n,ncol=P)

#use loop to generate permutations
for(i in 1:P){
  PermSamples[,i]<-sample(variable,size=n,replace=TRUE)
}
#look at first 5 columns
PermSamples[,1:5]
#need to add back in labels here

#make a loop to calculate the test-stats for each sample
Perm.test.stat1<-rep(0,P)
#loop through and calculate test stat
for(i in 1:P){
  Perm.test.stat1[i]<-abs(mean(PermSamples[1:100,i])-
                            mean(PermSamples[101:200,i]))
}
max(Perm.test.stat1)#since not a single value was greater than the true difference, the p-value =0
#May need to use the actual bootstrap estimates, not the mean of the bootstrap estimates

####################################################
####################################################
#old code
####################################################
####################################################
HTdf=heating_data[1353:1418,]
#396:461
  attach(HTdf)
    Temperature=HTdf[,which(colnames(HTdf)=="temperature")]
    FvFm=HTdf[,which(colnames(HTdf)=="fv_fm")]
    id=HTdf[,which(colnames(HTdf)=="Unique_ID")]#this sets the species grouping
    cof=coef(lm(logit(HTdf$fv_fm)~HTdf$temperature)) 
    #Fit a non linear least squares model to the FvFm and Temperature data
    HT.model <- nls(HTdf$fv_fm ~ theta1/(1 + exp(-(theta2 + theta3*HTdf$temperature))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                    trace=F, control=list(maxiter=5000, tol=0.01))#had to relax the tolerance to get model convergence, original tolerance is 1e-3
    
    #Use the parameter estimates (coef(HT.model)[#])from the HT.model to predict a new fit based on a heat treatments from 23-62 degrees celcius. Here, # = 1:3.
    y<-coef(HT.model)[1]/(1+exp(-(coef(HT.model)[2]+coef(HT.model)[3]*seq(23,62)))) 
    
    #Calculate half of the control Fv/Fm & a 95% reduction in FvFm with reference to control
    half=mean(na.omit(fv_fm[which(temperature==23)]))/2  
    nine5=mean(na.omit(fv_fm[which(temperature==23)]))*0.05
    fifteen=mean(na.omit(fv_fm[which(temperature==23)]))*0.85
    #creates the true mean estimates when all data are included
    nine5true=(-log((coef(HT.model)[1]/nine5)-1)-coef(HT.model)[2])/coef(HT.model)[3] 
    fiftytrue=(-log((coef(HT.model)[[1]]/half)-1)-coef(HT.model)[[2]])/coef(HT.model)[[3]]  
    #Tcrittrue  =(-log((coef(HT.model)[1]/fifteen)-1)-coef(HT.model)[2])/coef(HT.model)[3]  
    #special approach to estimating tcrit as this is 15% of the max slope
    #Use model to predict changes in FvFm & make new dataframe
    predicttrue=data.frame(x=seq(23,62,length.out=80),y=coef(HT.model)[1]/(1+exp(-(coef(HT.model)[2]+coef(HT.model)[3]*seq(23,62,length.out=80)))) ) #create a dataframe of predictions
    df1true=cbind(predicttrue[-1,], predicttrue[-nrow(predicttrue),])[,c(3,1,4,2)]
    #Use new dataframe to estimate the slope at between each 1-degree interval
    df1true$slp=as.vector(apply(df1true, 1, function(x) summary(lm((x[3:4]) ~ x[1:2])) [[4]][[2]] ))
    slp.at.tcrit=round(min(df1true$slp), 3)*.15 #Determine where slope is 15% of max slope & round
    #Estimate the FvFm at which the slope is 15% of max slope & less than T50
    fvfv.at.tcrit=df1true[which(abs(df1true[which(df1true[,1]<fiftytrue),]$slp-slp.at.tcrit)==min(abs(df1true[which(df1true[,1]<fiftytrue),]$slp-slp.at.tcrit))),][1,3]
    Tcrit=(-log((coef(HT.model)[[1]]/fvfv.at.tcrit)-1)-coef(HT.model)[[2]])/coef(HT.model)[[3]] # Estimate the temperatureat which the slope is 15% of max slope
detach(HTdf)
    #Step2 - bootstrap estimates of tcrit, t50 and t95
    #95 Confidence Interval
    #something is wrong on line 82 - I think Temperature and FvFm have to be vectors, not dataframes
    FvFm<-FvFm[['fv_fm']]
    Temperature<-Temperature[['temperature']]
    boots=100
    predict.boot=matrix(NA,40, boots)#the 40 represents the number of 1 degree intervals between the control and 62, 23-62
    T95=T50=Tcrit=c()
    for(k in 1:boots){
      #print(k)
      srows <- sample(1:length(Temperature), length(Temperature),replace=TRUE)#sample from 1:66 with replacement for each temperature
      
      if(class(try(nls(FvFm[srows] ~ theta1/(1 + exp(-(theta2 + theta3*Temperature[srows]))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                       trace=F, control=list(maxiter=1000, tol=.001)), silent=T)[[1]])=="nlsModel")
      {HT.model <- nls(FvFm[srows] ~ theta1/(1 + exp(-(theta2 + theta3*Temperature[srows]))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                        trace=F, control=list(maxiter=1000, tol=0.001))
      predict.boot[,k]=coef(HT.model)[1]/(1+exp(-(coef(HT.model)[2]+coef(HT.model)[3]*seq(23,62)))) #seq part means predictions from 23 to 62 degrees
      #Estimate T95
      T95[k]=(-log((coef(HT.model)[1]/nine5)-1)-coef(HT.model)[2])/coef(HT.model)[3] 
      
      #Estimate T50
      T50[k]=(-log((coef(HT.model)[[1]]/half)-1)-coef(HT.model)[[2]])/coef(HT.model)[[3]]
      T50k=(-log((coef(HT.model)[[1]]/half)-1)-coef(HT.model)[[2]])/coef(HT.model)[[3]]
      #Use model to predict changes in FvFm & make new dataframe
      predict=data.frame(x=seq(23,62,length.out=80),y=coef(HT.model)[1]/(1+exp(-(coef(HT.model)[2]+coef(HT.model)[3]*seq(23,62,length.out=80)))) ) #create a dataframe of predictions
      df1=cbind(predict[-1,], predict[-nrow(predict),])[,c(3,1,4,2)]
      #Use new dataframe to estimate the slope at between each 1-degree interval
      df1$slp=as.vector(apply(df1, 1, function(x) summary(lm((x[3:4]) ~ x[1:2])) [[4]][[2]] ))
      slp.at.tcrit=round(min(df1$slp), 3)*.15 #Determine where slope is 15% of max slope & round
      #Estimate the FvFm at which the slope is 15% of max slope & less than T50
      fvfv.at.tcrit=df1[which(abs(df1[which(df1[,1]<T50k),]$slp-slp.at.tcrit)==min(abs(df1[which(df1[,1]<T50k),]$slp-slp.at.tcrit))),][1,3]
      Tcrit[k]=(-log((coef(HT.model)[[1]]/fvfv.at.tcrit)-1)-coef(HT.model)[[2]])/coef(HT.model)[[3]] # Estimate the temperatureat which the slope is 15% of max slope
      
      }else{(class(try(nls(FvFm ~ theta1/(1 + exp(-(theta2 + theta3*Temperature))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                           data=data2[srows,], trace=F, control=list(maxiter=1000, tol=0.001)), silent=T)[[1]])=="list")
        predict.boot[,k]=NA
        T95[k]=NA
        T50[k]=NA
        Tcrit[k]=NA }}
    
    FvFm.boot=t(apply(predict.boot, 1, function(x){quantile(x,c(0.025,0.5,0.975),na.rm=T)}))
    
    Tcrit.ci=quantile(Tcrit,c(0.025,0.5,0.975),na.rm=T)
    T50.ci=quantile(T50,c(0.025,0.975),na.rm=T)
    T95.ci=quantile(T95,c(0.025,0.975),na.rm=T)
    #step 3 - compare bootstraps to true estimate using permutation test
    TcritLT<-Tcrit
    TcritOV<-Tcrit
    #calculate p-value
    #True value
    true<-48.12-43.73
    diff<-TcritLT-TcritOV#the difference between every bootstrapped run for Acer and Celtis
    (length(which(abs(diff)>=true)))/100    
    hist( TcritLT, col=rgb(0,0,1,1/4), xlim=c(40,52))
hist(TcritOV,col=rgb(1,0,0,1/4),add=T)    
