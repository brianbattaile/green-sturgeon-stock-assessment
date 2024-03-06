#Data For Table 5 and 6
library("unmarked")
library("nmixgof")
library("tidyverse")

# load the data
source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020)
Count2021 = load_data(Count2021)
Count2022 = load_data(Count2022)

#Remove rows and colums with all NA's
Data1<-Count2020
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3

UnMarDF<-unmarkedFramePCount(y=DATA)

#Poisson Model
Pmodel<-pcount(~1 ~1,data=UnMarDF, K=1000, mixture="P")
summary(Pmodel)
Pmean<-exp(Pmodel@estimates[1]@estimates)#Mean estimate
Abundance<-exp(Pmodel@estimates[1]@estimates)*dim(UnMarDF@y)[1]# Abundance estimate
DetProb<-exp(Pmodel@estimates[2]@estimates)/(1+exp(Pmodel@estimates[2]@estimates))#Binomial Detection Probability
EstCounts<-Abundance*DetProb
Pmean;Abundance;DetProb;EstCounts

#ZIP Model
ZIPmodel<-pcount(~1 ~1,data=UnMarDF, mixture="ZIP")
summary(ZIPmodel)
#logit back transform=1/(1+exp(-y))
ZIPmean<-exp(ZIPmodel@estimates[1]@estimates)#Mean estimate
Abundance<-exp(ZIPmodel@estimates[1]@estimates)*dim(UnMarDF@y)[1]# Abundance estimate
DetProb<-1-(1/(1+exp(ZIPmodel@estimates[2]@estimates)))#Binomial Detection Probability
ZeroInf<-1/(1+exp(ZIPmodel@estimates[3]@estimates))#Zero Inflation
EstCounts<-Abundance*DetProb*ZeroInf
ZIPmean;Abundance;DetProb;ZeroInf;EstCounts

#Negative Binomial Model
NBmodel<-pcount(~1 ~1, data=UnMarDF, K=10000, mixture="NB")
summary(NBmodel)
NBmean<-exp(NBmodel@estimates[1]@estimates)#Mean estimate
NBDisp<-exp(NBmodel@estimates[3]@estimates)#Disp estimate (Size)
exp(NBmodel@estimates[1]@estimates)*dim(UnMarDF@y)[1]# Abundance estimate
confint(NBmodel, type='state', level=0.95)# get confidence intervals for state and det
DetProb<-exp(NBmodel@estimates[2]@estimates)/(1+exp(NBmodel@estimates[2]@estimates))#Binomial Detection Probability
EstCounts<-NBmean*DetProb*30
EstTotalPop<-NBmean*30
NBDisp;NBmean;DetProb;EstCounts;EstTotalPop


