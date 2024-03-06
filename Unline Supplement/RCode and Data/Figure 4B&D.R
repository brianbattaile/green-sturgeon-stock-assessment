library(tidyverse)

# load the functions
source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020)
Count2021 = load_data(Count2021)
Count2022 = load_data(Count2022)

#Remove rows and colums with all NA's
Data1<-Count2020
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3

C2020<-rep(DATA,11111)
ZIP<-rbinom(1000000,1,.700)*rbinom(1000000,rpois(1000000,33.7),0.705)
Pois<-rbinom(1000000,rpois(1000000,21.64989),0.7464837)
NegBin<-rbinom(1000000,rnbinom(1000000,size=.2294676,mu=39.2975),0.4229392)#Detectability K=221

df<-data.frame(Value=c(C2020,
                       Pois,
                       ZIP
                       #NegBin
),
Dist=c(rep("Data",999990), #999990
       rep("Poisson",1000000),
       rep("ZIP",1000000)
       #rep("Negative Binomial",1000000)#NB38
),
row.names=NULL
) 

Figure<-
     ggplot(df, aes(x=Value, y=..density.., color = Dist, fill=Dist)) +
     stat_bin(position="identity",center=0,binwidth=1, alpha=.2) +
     #geom_density(alpha=0.2, trim=T, adjust=.5) +
     xlim(-1,125) +
     #ylim(0,200000)+
     labs(x="Number of Animals", y="Density", title="Poisson and ZIP Distributions", color = "Data Type", fill = "Data Type")

Figure + theme_bw() + 
     theme(legend.position = c(0.90, 0.5),legend.text = element_text(size = 10),legend.title = element_text(size = 0)) + #Figure 3 and 4 Width 800 Height 500
     theme(plot.title = element_text(size = 20)) +
     theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12)) +
     theme(axis.title = element_text(size = 16)) #Figure 3
              