#https://sourceforge.net/projects/mcmc-jags/files/
#testjags()
#install.packages(c("runjags", "coda", "ggplot2"))
options(StringsAsFactorys=FALSE,max.print=6000000)
library("runjags")
library("ggplot2")

source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020)
Count2021 = load_data(Count2021)
Count2022 = load_data(Count2022)
Counts<-array(c(Count2020,Count2021,Count2022),c(88,7,3))

#CountsR<-Counts[-c(1,5,7,10,13,14,24,25,28,37,38,50,59,63,70,72,73,83),,] #remove locations where all three have no data
#CountsRR<-Counts[-c(1,5:7,10,13,14,16,17,19:21,24,25,28,32:35,37,38,40,42,44,47,49:51,54:57,59,63:65,67:88),-c(4:7),] #remove locations where any one has no data
dim(Counts)[3]#number of layers in the array
DATA<-Counts #CountsRR
numsur<-7-rowSums(is.na(DATA))#ragged matix indicies

jags.model.string <- "
model {
#Priors
for(k in 1:n.years)
{
alpha.lam[k]~dnorm(0,1) #Eq.14
beta[k]~dunif(0,1) #Eq.18
}

# Abundance noise
for (i in 1:n.sites){
  eps[i]~dnorm(0, tau.lam) #Eq.11
}
tau.lam<-1/(sd.lam*sd.lam) #Eq.12
sd.lam~dunif(0,5)#Eq.13

#Detection noise
tau.p<-1/(sd.p*sd.p) #Eq.19
sd.p~dunif(0,3) #Eq.20

# abundance component
for (i in 1:n.sites)
{
  for (k in 1:n.years)
{
  N[i,k] ~ dpois(lambda[i,k]) #Eq.9
  log(lambda[i,k])<-alpha.lam[k]+eps[i] #Eq.10
  # detection component
   for (j in 1:n.surveys)
  {
    Y[i,j,k] ~ dbin(p[i,j,k], N[i,k]) #Eq.15
    p[i,j,k]<-exp(lp[i,j,k])/(1+exp(lp[i,j,k])) #Eq.16 logit back transform
    lp[i,j,k]~dnorm(beta[k], tau.p) #Eq.17 See v95i02 for linear model example
    
    #Assess model fit using Chi-squared discrepeancy
    #Compute fit statistic E for observed data
    eval[i,j,k]<-p[i,j,k]*N[i,k] #Eq.22 
    E[i,j,k]<-pow((Y[i,j,k]-eval[i,j,k]),2)/(eval[i,j,k]+0.5) #Eq.23
    #Generate replicate data and compute fit stats for them
    y.new[i,j,k]~dbin(p[i,j,k],N[i,k]) #Eq.24
    E.new[i,j,k]<-pow((y.new[i,j,k]-eval[i,j,k]),2)/(eval[i,j,k]+0.5) #Eq.25
  } # close j loop
  ik.p[i,k]<-mean(p[i,,k]) 
} # Close the k loop
} # close i loop
#Derived quantities
for(k in 1:n.years)
     {
     totalN[k]<-sum(N[,k])  #Estimate total pop. size across all sites
     mean.abundance[k]<-mean(lambda[,k])
     mean.N[k]<- mean(N[,k])
     mean.detection[k]<-mean(ik.p[,k])
     #mean.eps[k]<-mean(eps[,k])
     } # close 2nd k loop
fit<-sum(E[,,])
fit.new<-sum(E.new[,,]) 
} # close model loop
"
     
# Parameters to monitor
params<-c("totalN","alpha.lam", "sd.lam", "beta",
         "mean.eps", "tau.lam", "mean.abundance", "mean.N", "mean.detection",
         "fit", "fit.new","sd.p","tau.p", "N", "lambda" ,"eps", "ik.p", "p")
     
jags.data <- list(Y = DATA, n.sites = NROW(DATA), n.surveys = NCOL(DATA), n.years=dim(DATA)[3])
     
# initial values
N.init <- DATA # initial count values
N.init[is.na(N.init)] <- 1 # 1 clean up NA's
N.init <- apply(N.init, c(1,3), max) + 1 # zero values cause trouble
inits <- function() list(N = N.init)
     
nc <- 3; na <- 1000; nb <- 1000; ni <- 1000; nt <- 10
     
# run jags
out.jags.ODBpoisMY <- run.jags(model = jags.model.string, data = jags.data,
                           monitor = params, n.chains = nc, inits = inits,
                           burnin = nb, adapt = na, sample = ni, thin = nt,
                           modules = "glm on", method = "parallel")
     
######RESULTS######## R^=psrf=potential scale reduction factor
#Convergence statistic that should be close to 0 (<1.05)
#print(out.jags.pois, dig=3)
SumStatODBPoisMY<-as.data.frame(round(summary(out.jags.ODBpoisMY), 3)[ , c(1:5, 9, 11)]) 
SumStatODBPoisMY[1:300,]
MN<-SumStatODBPoisMY[grep("^N",rownames(SumStatODBPoisMY)),2]
sum(MN[1:88]);sum(MN[89:176]);sum(MN[177:264])#Abundance by year

#plots the summary plots for each monitored parameter, trace, ecdf, posterior historgram & autocor + the autocorr 
#plot(out.jags.BODPoisMY)
#Specify plots for a parameter, choices are "trace", "density", ecdf", "histogram", "autocorr", "key", "crosscorr"
#plot(out.jags.BODPoisMY,plot.type=c("histogram", "trace"), vars="beta",)

#Plot the Goodness of Fit Test, should look like a normally distributed cloud about the 1:1 line
FIT<-c(unlist(out.jags.ODBpoisMY$mcmc[1][,"fit"]),unlist(out.jags.ODBpoisMY$mcmc[2][,"fit"]),unlist(out.jags.ODBpoisMY$mcmc[3][,"fit"]))
FITNEW<-c(unlist(out.jags.ODBpoisMY$mcmc[1][,"fit.new"]),unlist(out.jags.ODBpoisMY$mcmc[2][,"fit.new"]),unlist(out.jags.ODBpoisMY$mcmc[3][,"fit.new"]))
plot(FIT, FITNEW, 
     main="", 
     xlab="Discrepancy actual data",
     ylab="Discrepancy replicate data",
     frame.plot=FALSE
)
abline(0,1,lwd=2,col="red")
