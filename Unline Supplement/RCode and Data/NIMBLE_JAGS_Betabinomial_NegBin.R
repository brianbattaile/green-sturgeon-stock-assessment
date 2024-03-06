options(StringsAsFactorys=FALSE,max.print=600000)
library(nimble)
library(MCMCvis)
library("runjags")

source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020) #median N estimates are max counts, mean estiamtes have high values of areas of 0 counts.  median p values are exact percentages of the max.  individual N's do not add up to TotalN.
Count2021 = load_data(Count2021) # ALL-median N estiamtes are max counts, no variation, p values seem ok
               #DATA[-c(6,26,34),] median estimates of 0's are non-zero.  p values for 0's are 0.  individual N's do not add up to TotalN.
#DATA<-DATA[-c(6,11,12,25,26,34),] median estimates of 0's are non-zero.  p values for 0's are 0.  individual N's do not add up to TotalN.
Count2022 = load_data(Count2022) #Nimble-no variation in estimates of N, p values seem ok

#Remove rows and colums with all NA's
Data1<-Count2021
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3
#2021 Variations
#DATA<-DATA[-c(6,11,12,25,26,34),]#No variation in estimates of N
#DATA<-DATA[-c(6,26,34),]

###################
######NIMBLE#######
###################

#####MODEL#######
NMixBBNB_code<-nimbleCode({
     #Priors
     #Detection prior
     alpha.p~dunif(0,20)#dgamma(0.01,0.01)#Eq.7
     beta.p~dunif(0,20)#dgamma(0.01,0.01)#Eq.8
     #Abundance prior
     lambda ~ dunif(0,1000)
     disp ~ dunif(0,20)
     NBvar<-1/disp #r in equlation in text of paper described in line below
     NBmean <- NBvar / (NBvar + lambda) # Eq in text between Eq.3 and Eq.4-overdispersion p_NB=r/(r+lambda_NB)
     # Abundance component
     for(i in 1:n.sites){
          N[i] ~ dnegbin(NBmean,NBvar) #Eq.3
          # Detection component
          for (j in 1:n.surveys)
          {
               Y[i, j] ~ dbinom(p[i, j], N[i])#Eq.2
               p[i, j] ~ dbeta(alpha.p,beta.p)#Eq.6
          }
       ik.p[i]<-mean(p[i,1:n.surveys])
     } 
     # Derived quantities
     totalN<-sum(N[])
     mean.p<-mean(ik.p[1:n.sites])
})#close nimble code

###SET UP AND RUN MODEL######

parameters.to.save<-c("lambda", "totalN", "mean.p", "alpha.p", "beta.p", "disp", "NBvar", "NBmean", "N", "p")

nimble.data <-list(Y=DATA, n.sites=nrow(DATA), n.surveys=ncol(DATA))

N.init <- DATA # initial count values
N.init[is.na(N.init)] <- 1 # clean up NA's
N.init <- apply(N.init, 1, max) + 1 # zero values cause trouble
initial.values<-function()list(N = N.init)

n.iter <- 6000
n.burnin <- 1000
n.thin <- 10
n.chains <- 3

mcmc.output <- nimbleMCMC(code = NMixBBNB_code,
                          constants = nimble.data,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = 3,
                          samplesAsCodaMCMC = TRUE
                          #summary = TRUE,
                          #WAIC = TRUE
)

###################
#######JAGS########
###################

jags.model.string <- "
model {
  #Priors
  #Detection prior
  alpha.p~dgamma(0.01,0.01)#dunif(0,20)#Eq.7
  beta.p~dgamma(0.01,0.01)#dunif(0,20)#Eq.8
  #Abundance prior
  lambda ~ dunif(0,1000)
  disp ~ dunif(0,20)
  NBvar<-1/disp #r in equlation in text of paper described in line below
  NBmean <- NBvar / (NBvar + lambda) # Eq in text between Eq.3 and Eq.4-overdispersion p_NB=r/(r+lambda_NB)
  # Abundance component
  for(i in 1:n.sites){
    N[i] ~ dnegbin(NBmean,NBvar)
    # Detection component
    for (j in 1:n.surveys)
    {
      Y[i, j] ~ dbinom(p[i, j], N[i])#Eq.3
      p[i, j] ~ dbeta(alpha.p,beta.p)#Eq.6
      
     #Assess model goodness of fit
     #Compute fit statistic E for observed data
      eval[i,j]<-p[i,j]*N[i] #Eq.22 expected values
      E[i,j]<-pow((Y[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.23 Sum of Squares sort of thing comparing Data to Model Fit 
     #Generate replicate data and compute fit stats for them
      y.new[i,j]~dbin(p[i,j],N[i]) #Eq.24
      E.new[i,j]<-pow((y.new[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) #Eq.25 Comparing Random draws from model to Model Fit
    }
    ik.p[i]<-mean(p[i,])
  } 
  # Derived quantities
  totalN<-sum(N[])
  mean.p<-mean(ik.p[])
  fit<-sum(E[,])
  fit.new<-sum(E.new[,]) 
}"

###SET UP AND RUN MODEL######

# Parameters to monitor
params<-c("totalN", "mean.p", "alpha.p", "beta.p", "lambda", "NBmean", "NBvar", "disp", "fit", "fit.new", "N", "p") 

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
ni <- 1000 #kept samples
nt <- 10    #thin

# run jags
out.jags.NBBB <- run.jags(model = jags.model.string,
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

#####RESULTS######
#JAGS
SumStatJAGS2020Unif<-as.data.frame(round(summary(out.jags.NBBB), 3)[ , c(1:5, 9, 11)]) 
#NIMBLE
SumStatNIMBLE2020<-MCMCsummary(object = mcmc.output,round = 2,params = c("all"),Rhat = TRUE,n.eff = TRUE)
tail(MCMCchains(object = mcmc.output))
MCMCplot(object = mcmc.output,params=c("alpha.p","beta.p"))


#PRINT RESULTS
SumStatJAGS2020Unif
SumStatNIMBLE2020Unif

#Plot Results
plot(out.jags.NBBB)
MCMCtrace(object = mcmc.output, pdf = FALSE, ind = TRUE, Rhat = TRUE, n.eff = TRUE, params = "beta.p")

