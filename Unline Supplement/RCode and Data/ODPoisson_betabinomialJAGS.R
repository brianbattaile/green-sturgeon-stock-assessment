#https://sourceforge.net/projects/mcmc-jags/files/
#testjags()
#install.packages("runjags")
options(StringsAsFactorys=FALSE,max.print=600000)
library("runjags")
library(tidyverse)

# load the functions
source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020)
Count2021 = load_data(Count2021)
Count2022 = load_data(Count2022)

#Remove rows and colums with all NA's
Data1<-Count2021
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3

jags.model.string <- " 
model {
# priors
# Abundance prior
     alpha.lam~dnorm(0,1) #Eq.14 dnorm(0,3)

# Abundance noise
     for (i in 1:n.sites){
          eps[i]~dnorm(0, tau.lam) #Eq.11
     }
     
     tau.lam<-1/(sd.lam*sd.lam) #Eq.12
     sd.lam~dunif(0,5) #Eq.13
# Detection
  alpha.p~dgamma(0.01,0.01) #Eq.7
  beta.p~dgamma(0.01,0.01) #Eq.8

# abundance component
for (i in 1:n.sites)
{
     N[i] ~ dpois(lambda[i]) #Eq.9 poisson
     log(lambda[i])<-alpha.lam+eps[i] #Eq.10
# detection component
     for (j in 1:n.surveys)#n.surveys numsur[i]
     {
          Y[i, j] ~ dbin(p[i,j], N[i]) #Eq.2
          p[i, j] ~ dbeta(alpha.p,beta.p) #Eq.6
          
          #Assess model fit using Chi-squared discrepeancy
          #Compute fit statistic E for observed data
          eval[i,j]<-p[i,j]*N[i] #Eq.22 expected values
          E[i,j]<-pow((Y[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.23 Sum of Squares sort of thing comparing Data to Model Fit 
     
          #Generate replicate data and compute fit stats for them
          y.new[i,j]~dbin(p[i,j],N[i]) #Eq.24
          E.new[i,j]<-pow((y.new[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.25 Comparing Random draws from model to Model Fit
     } # close j loop
     ik.p[i]<-mean(p[i,])
} # close i loop
#Derived quantities
totalN<-sum(N[])  #Estimate total pop. size across all sites
mean.abundance<-mean(lambda[])
mean.N<- mean(N[])
mean.detection<-mean(ik.p[])
mean.eps<-mean(eps[])
fit<-sum(E[,])
fit.new<-sum(E.new[,]) 
} # close model loop
"
     
# Parameters to monitor
params<-c("totalN", "mean.abundance", "mean.N", "alpha.lam", "mean.eps", "sd.lam","tau.lam",
          "alpha.p", "beta.p", "mean.detection","fit", "fit.new",
          "N", "p", "lambda", "ik.p", "eps")#These ones are site and survey specific....so lots of each.
     
# jags data
jags.data <- list(Y = DATA, n.sites = NROW(DATA), n.surveys = NCOL(DATA)) 
     
# initial values
N.init <- DATA # initial count values 
N.init[is.na(N.init)] <- 1 # clean up NA's
N.init <- apply(N.init, 1, max) + 1 # zero values cause trouble
inits <- function() list(N = N.init)
     
# set run parameters
nc <- 3; na <- 1000; nb <- 1000; ni <- 10000; nt <- 10
     
# run jags
out.jags.BBODpois <- run.jags(model = jags.model.string, data = jags.data,
                           monitor = params, n.chains = nc, inits = inits,
                           burnin = nb, adapt = na, sample = ni, thin = nt,
                           modules = "glm on", method = "parallel")
     
######RESULTS######## R^=psrf=potential scale reduction factor convergence statistic that should be close to 0 (<1.05)
SumStatBBODPois<-as.data.frame(round(summary(out.jags.BBODpois), 3)[ , c(1:5, 9, 11)]) 
SumStatBBODPois
#plots the summary plots for each monitored parameter, trace, ecdf, posterior historgram & autocor + the autocorr 
#plot(out.jags.BBODPois)
#Specify plots for a parameter, choices are "trace", "density", ecdf", "histogram", "autocorr", "key", "crosscorr"
#plot(out.jags.BBODPois,plot.type=c("histogram", "trace"), vars="beta",)

#Plot the Goodness of Fit Test, should look like a normally distributed cloud about the 1:1 line
FIT<-c(unlist(out.jags.BBODpois$mcmc[1][,"fit"]),unlist(out.jags.BBODpois$mcmc[2][,"fit"]),unlist(out.jags.BBODpois$mcmc[3][,"fit"]))
FITNEW<-c(unlist(out.jags.BBODpois$mcmc[1][,"fit.new"]),unlist(out.jags.BBODpois$mcmc[2][,"fit.new"]),unlist(out.jags.BBODpois$mcmc[3][,"fit.new"]))
plot(FIT, FITNEW, 
     main="", 
     xlab="Discrepancy actual data",
     ylab="Discrepancy replicate data",
     frame.plot=FALSE
)
abline(0,1,lwd=2,col="red")

