
HTdf=heating_data[1:66,]
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
    