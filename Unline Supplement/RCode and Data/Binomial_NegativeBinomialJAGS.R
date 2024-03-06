#https://sourceforge.net/projects/mcmc-jags/files/
#testjags()
#install.packages("runjags")
library("runjags")
library("tidyverse")

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

jags.model.string <- "
model {
# priors
lambda ~ dunif(0,10000)
disp ~ dunif(0,20)
p ~ dbeta(1,1)
NBvar<-1/disp #r in equlation in text of paper described in line below
NBmean <- NBvar / (NBvar + lambda) # Eq in text between Eq.3 and Eq.4-overdispersion p_NB=r/(r+lambda_NB)
# abundance component
for (i in 1:n.sites)
{
  N[i] ~ dnegbin(NBmean,NBvar) #Eq.3 negative binomial
  # detection component
  for (j in 1:n.surveys)
  {
    Y[i, j] ~ dbin(p, N[i]) #Eq.2
    
    #Assess model fit using Chi-squared discrepeancy
    #Compute fit statistic E for observed data
    eval[i,j]<-p*N[i] #Eq.22 expected values
    E[i,j]<-pow((Y[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.23
    
    #Generate replicate data and compute fit stats for them
    y.new[i,j]~dbin(p,N[i]) #Eq.24
    E.new[i,j]<-pow((y.new[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.25
  } # close j loop
} # close i loop
#Derived quantities
totalN<-sum(N[])  #Estimate total pop. size across all sites
mean.N<- mean(N[])
fit<-sum(E[,])
fit.new<-sum(E.new[,])
} # close model loop
"
# Parameters to monitor
params<-c("totalN", "lambda", "mean.N", "p", "NBvar", "NBmean", "disp", "fit", "fit.new", "N")

# jags data
jags.data <- list(Y = DATA, n.sites = NROW(DATA), n.surveys = NCOL(DATA)) 

# initial values
N.init <- DATA # initial count values
N.init[is.na(N.init)] <- 1 # clean up NA's
N.init <- apply(N.init, 1, max) + 1 # zero values cause trouble
inits <- function() list(N = N.init)

# set run parameters
nc <- 3; na <- 1000; nb <- 1000; ni <- 10000; nt <- 10 #1,000,000-nt=200   100,000-nt=20    10000-nt=10

# run jags
out.jags.BNB <- run.jags(model = jags.model.string, data = jags.data,
                       monitor = params, n.chains = nc, inits = inits,
                       burnin = nb, adapt = na, sample = ni, thin = nt,
                       modules = "glm on", method = "parallel")

######RESULTS######## R^=psrf=potential scale reduction factor convergence statistic that should be close to 0 (<1.05)
SumStatBNB<-as.data.frame(round(summary(out.jags.BNB), 3)[ , c(1:5, 9, 11)]) 
SumStatBNB
#plots the summary plots for each monitored parameter, trace, ecdf, posterior historgram & autocor + the autocorr 
plot(out.jags.BNB)
#Specify plots for a parameter, choices are "trace", "ecdf", "histogram", "autocorr", "key", "crosscorr"
plot(out.jags.BNB,plot.type=c("histogram", "trace"), vars="lambda",)

#Plot the Goodness of Fit Test, should look like a normally distributed cloud about the 1:1 line
FIT<-c(unlist(out.jags.BNB$mcmc[1][,"fit"]),unlist(out.jags.BNB$mcmc[2][,"fit"]),unlist(out.jags.BNB$mcmc[3][,"fit"]))
FITNEW<-c(unlist(out.jags.BNB$mcmc[1][,"fit.new"]),unlist(out.jags.BNB$mcmc[2][,"fit.new"]),unlist(out.jags.BNB$mcmc[3][,"fit.new"]))
plot(FIT, FITNEW, 
     main="",
     xlim=c(0,150),
     xlab="Discrepancy actual data",
     ylab="Discrepancy replicate data",
     frame.plot=FALSE
)
abline(0,1,lwd=2,col="red")

