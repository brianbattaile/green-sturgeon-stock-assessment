#https://sourceforge.net/projects/mcmc-jags/files/
#testjags()
#install.packages("runjags")
library("runjags")
library("tidyverse")

# load the functions
source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020)
Count2021 = load_data(Count2021)
Count2021BBNB = load_data(Count2021BBNB)
Count2022 = load_data(Count2022)

#Remove rows and colums with all NA's
Data1<-Count2021BBNB
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3
#2021 Variations
#DATA<-DATA[-c(6,11,12,25,26,34),]#Removes all sites with one or more missing observations
#DATA<-DATA[-c(6,26,34),]#removes sites with only one observation

jags.model.string <- "
model {
  # Priors
  #Detection prior
  alpha.p~dgamma(0.01,0.01) #Eq.7
  beta.p~dgamma(0.01,0.01) #Eq.8
  #Abundance prior
  lambda ~ dunif(0,1000)
  disp ~ dunif(0,20)
  NBvar<-1/disp #r in equlation in text of paper described in line below
  NBmean <- NBvar / (NBvar + lambda) # Eq in text between Eq.3 and Eq.4-overdispersion p_NB=r/(r+lambda_NB)

  for(i in 1:n.sites){
    N[i] ~ dnegbin(NBmean,NBvar) #Eq.3
    
    # Detection component
    for (j in 1:n.surveys)
    {
      Y[i, j] ~ dbinom(p[i, j], N[i]) #Eq.2
      p[i, j] ~ dbeta(alpha.p,beta.p) #Eq.6

      # Assess model fit using Chi-squared discrepeancy
      # Compute fit statistic E for observed data
      eval[i,j]<-p[i, j]*N[i] #Eq.22 Values our model fit found for the data...eval=Expected VALue
      E[i,j]<-pow((Y[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.23 difference between data and Expected VALue
      
      # Generate replicate data and compute fit stats for them
      y.new[i,j]~dbinom(p[i, j],N[i]) #Eq.24 random samples from model fit
      E.new[i,j]<-pow((y.new[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.25 diff between random samples and Expected VALue
    } 
  } 
  # Derived quantities
  # Estimate total pop. size across all sites
  totalN<-sum(N[])  
  mean.N<- mean(N[])
  mean.p <- mean(p[,])
  p.derived<-alpha.p/(alpha.p+beta.p) #derived detection probability
  rho.derived<-1/(alpha.p+beta.p+1) #derived correlation coefficient 
  fit<-sum(E[,])
  fit.new<-sum(E.new[,])
}"

# Parameters to monitor
params<-c("totalN", "mean.N", "alpha.p", "beta.p", "mean.p", "p.derived", "rho.derived", "lambda", "NBmean", "NBvar", "disp", "fit", "fit.new", "N", "p") 

jags.data <- list(Y = DATA, n.sites = NROW(DATA), n.surveys = NCOL(DATA)) 

# initial values
N.init <- DATA # initial count values 
N.init[is.na(N.init)] <- 1 # clean up NA's
N.init <- apply(N.init, 1, max) + 1 # zero values cause trouble
inits <- function() list(N = N.init)

# set run parameters
nc <- 3    #number of chains
na <- 1000 #adaptation
nb <- 1000 #burn in
ni <- 5000 #kept samples
nt <- 10    #thin

# run jags
out.jags.BBNB <- run.jags(model = jags.model.string,
                     data = jags.data,
                     monitor = params,
                     inits = inits,
                     n.chains = nc,
                     burnin = nb,
                     adapt = na,
                     sample = ni,
                     thin = nt,
                     modules = "glm on",
                     method = "parallel")

###### RESULTS ########
# R^=psrf=potential scale reduction factor convergence statistic
# that should be close to 0 (<1.05)
# Print summary
SumStatBBNB<-as.data.frame(round(summary(out.jags.BBNB), 3)[ , c(1:5, 9, 11)]) 
SumStatBBNB
# Plots the summary plots for each monitored parameter, trace,
# ecdf, posterior historgram & autocor + the autocorr 
# plot(out.jags.pois)

# Specify plots for a parameter, choices are "trace", "ecdf", "histogram", "autocorr", "key", "crosscorr"
# plot(out.jags,plot.type=c("histogram", "trace"), vars="lambda",)

