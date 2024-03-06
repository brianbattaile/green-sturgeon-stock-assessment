#https://sourceforge.net/projects/mcmc-jags/files/
#testjags()
#install.packages(c("runjags", "coda", "ggplot2"))
options(StringsAsFactorys=FALSE,max.print=600000)
library("runjags")
library("tidyverse")

# load the functions
source("n_mixture_data_functions.R")
Count2022 = load_data(Count2022)
#Remove rows and colums with all NA's
Data1<-Count2022
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3

UD2022<-read.table("UpDownStream.txt", fill=TRUE, sep="\t", header=TRUE, row.names="Area")
UD2022<-as.matrix(UD2022)
rownames(UD2022)<-NULL
colnames(UD2022)<-NULL

numsur<-NCOL(DATA)-rowSums(is.na(DATA))

jags.model.string <-" 
model {
#Priors
#detection priors
  beta0~dnorm(0,1)
  beta1~dnorm(0,1)
# Abundance noise
  alpha.lam~dnorm(0,1) #Eq.14
  for (i in 1:n.sites){
    eps[i]~dnorm(0, tau.lam) #Eq.11
  }
  tau.lam<-1/(sd.lam*sd.lam) #Eq.12
  sd.lam~dunif(0,5) #Eq.13
# abundance component
for (i in 1:n.sites)
{
  N[i] ~ dpois(lambda[i]) #Eq.9 
  log(lambda[i])<-alpha.lam+eps[i] #Eq.10
  
  # detection component
  for (j in 1:numsur[i]) 
  {
    Y[i, j] ~ dbin(p[i,j], N[i]) #Eq.15
    p[i,j]<-exp(lp[i,j])/(1+exp(lp[i,j])) #Eq.16  Everyone else logit back transform
    lp[i,j]<-beta0 + beta1 * UD2022[i,j] #Eq.21 linear model for up or down stream 
    
    #Assess model fit using Chi-squared discrepeancy
    #Compute fit statistic E for observed data
    eval[i,j]<-p[i,j]*N[i] #Eq.22
    E[i,j]<-pow((Y[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.23

    #Generate replicate data and compute fit stats for them
    y.new[i,j]~dbin(p[i,j],N[i]) #Eq.24
    E.new[i,j]<-pow((y.new[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.25
  } # close j loop
  ik.p[i]<-mean(p[i,1:numsur[i]])
  fiti[i]<-sum(E[i,1:numsur[i]])
  fit.newi[i]<-sum(E.new[i,1:numsur[i]])
} # close i loop
#Derived quantities
totalN<-sum(N[])  #Estimate total pop. size across all sites
mean.abundance<-mean(lambda[])
mean.N<- mean(N[])
mean.detection<-mean(ik.p[])
#mean.eps<-mean(eps[])
fit<-sum(fiti)
fit.new<-sum(fit.newi)
} # close model loop
"
  
# Parameters to monitor
params<-c("totalN", "mean.abundance", "mean.N", "mean.eps", 
          "beta0", "beta1",  "mean.detection", "sd.lam", "tau.lam",
          "fit", "fit.new", "N", "lambda", "p", "eps")
  
# jags data
jags.data <- list(Y = DATA, n.sites = NROW(DATA), UD2022=UD2022, numsur=numsur)
  
  
# initial values
N.init <- DATA # initial count values
N.init[is.na(N.init)] <- 1 # clean up NA's
N.init <- apply(N.init, 1, max) + 1 # zero values cause trouble
inits <- function() list(N = N.init)
  
# set run parameters
nc <- 3; na <- 1000; nb <- 1000; ni <- 10000; nt <- 10
  
# run jags
out.jags.ODpoisCov <- run.jags(model = jags.model.string, data = jags.data,
                          monitor = params, n.chains = nc, inits = inits,
                          burnin = nb, adapt = na, sample = ni, thin = nt,
                          modules = "glm on", method = "parallel")
  
######RESULTS######## R^=psrf=potential scale reduction factor
#Convergence statistic that should be close to 0 (<1.05)
SumStatODPoisCov<-as.data.frame(round(summary(out.jags.ODpoisCov), 3)[ , c(1:5, 9, 11)])#First 13 monitored parameters 
SumStatODPoisCov
#plots the summary plots for each monitored parameter, trace, ecdf, posterior historgram & autocor + the autocorr 
#plot(out.jags.poisOD)
#Specify plots for a parameter, choices are "trace", "density", ecdf", "histogram", "autocorr", "key", "crosscorr"
#plot(out.jags.poisOD,plot.type=c("histogram", "trace"), vars="beta",)

FIT<-c(unlist(out.jags.ODpoisCov$mcmc[1][,"fit"]),unlist(out.jags.ODpoisCov$mcmc[2][,"fit"]),unlist(out.jags.ODpoisCov$mcmc[3][,"fit"]))
FITNEW<-c(unlist(out.jags.ODpoisCov$mcmc[1][,"fit.new"]),unlist(out.jags.ODpoisCov$mcmc[2][,"fit.new"]),unlist(out.jags.ODpoisCov$mcmc[3][,"fit.new"]))
plot(FIT, FITNEW, 
     main="", 
     xlab="Discrepancy actual data",
     ylab="Discrepancy replicate data",
     frame.plot=FALSE
)
abline(0,1,lwd=2,col="red")
