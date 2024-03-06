# load the data
source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020)
Count2021 = load_data(Count2021)
Count2022 = load_data(Count2022)

#Remove rows and colums with all NA's
Data1<-Count2022
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3

#Table 4 # Sites, SSS Sum Site Means and Sum Site Max
NumberSites<-NROW(DATA);NumberSites
numsur<-NCOL(DATA)-rowSums(is.na(DATA));
#Calculate the sum of the max count per Hole as a basic minimum population estimate
max_method<-sum(apply(DATA,1,max,na.rm=T));max_method #Equation 27
#Calculate the Abundance using a mean over holes and surveys.
mean_method<-sum(rowMeans(DATA, na.rm=T));mean_method#Equation 26 also mean(DATA,na.rm=T)*dim(DATA)[1] 
