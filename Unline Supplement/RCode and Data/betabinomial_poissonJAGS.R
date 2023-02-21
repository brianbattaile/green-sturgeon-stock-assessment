##### This is a testing program for n-mixnture models
options(StringsAsFactorys=FALSE,max.print=600000)
library("runjags")
library("tidyverse")

# load the functions
source("n_mixture_data_functions.R")
Count2020 = load_data(Count2020)
Count2021 = load_data(Count2021)
Count2022 = load_data(Count2022)

#Remove rows and colums with all NA's
Data1<-Count2022
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3

jags.model.string <- "
model {
  # Priors
  #Detection prior
  alpha.p~dgamma(0.01,0.01) #Eq.7
  beta.p~dgamma(0.01,0.01) #Eq.8
  #Abundance prior
  lambda ~ dgamma(0.005, 0.005) #dunif(0,10) #Other possibility

  for(i in 1:n.sites){
    N[i] ~ dpois(lambda) #Eq.1 poisson
    
    # Detection component
    for (j in 1:n.surveys)
    {
      Y[i, j] ~ dbinom(p[i, j], N[i]) #Eq.2
      p[i, j] ~ dbeta(alpha.p,beta.p) #Eq.6

      # Assess model fit using Chi-squared discrepeancy
      # Compute fit statistic E for observed data
      # Expected values
      eval[i,j]<-p[i, j]*N[i] # Eq.22 Values our model fit found for the data...eval=Expected VALue
      E[i,j]<-pow((Y[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) # Eq.23 difference between data and Expected VALue
      
      # Generate replicate data and compute fit stats for them
      y.new[i,j]~dbinom(p[i, j],N[i]) #Eq.24 random samples from model fit
      E.new[i,j]<-pow((y.new[i,j]-eval[i,j]),2)/(eval[i,j]+0.5) # Eq.25 diff between random samples and Expected VALue
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
params<-c("totalN", "mean.N", "alpha.p", "beta.p", "mean.p", "p.derived", "rho.derived", "lambda", "fit", "fit.new", "N", "p") 

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
out.jags.BBPois <- run.jags(model = jags.model.string,
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
SumStatBBPois<-as.data.frame(round(summary(out.jags.BBPois), 3)[ , c(1:5, 9, 11)]) 
SumStatBBPois



results = round(summary(out.jags), 3)[1:length(params)-1,] 
ratio = max_method/results["totalN", "Mode"]

print(results)
print(paste0("!!!!!!! max count method = ", max_method))
print(ratio)

# Plots the summary plots for each monitored parameter, trace,
# ecdf, posterior historgram & autocor + the autocorr 
# plot(out.jags.pois)

# Specify plots for a parameter, choices are "trace", "ecdf", "histogram", "autocorr", "key", "crosscorr"
# plot(out.jags,plot.type=c("histogram", "trace"), vars="lambda",)

# Get the eval data into a good format for plotting
FIT <- map(seq(1:nc), ~unlist(out.jags,BBPois$mcmc[.x][,"fit"])) %>% 
  unlist()
FITNEW <- map(seq(1:nc), ~unlist(out.jags.BBPois$mcmc[.x][,"fit.new"])) %>% 
  unlist() 
check_data = data.frame(fit = FIT,
                        new_fit = FITNEW)

ggplot(data = check_data, aes(x = fit, y = new_fit)) +
  labs(x = "Discrepancy actual data",
       y = "Discrepancy replicate data") +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red")

data_vector = c(data)
data_df<-data.frame(x = data_vector[!is.na(data_vector)]) %>% 
  mutate(type = "data")

fit_vector = seq(1:nc) %>% 
  map(~(out.jags.BBPois$mcmc[[.x]] %>% 
          data.frame() %>%
          select(starts_with("N")) %>% 
          as.matrix() %>% 
          c())) %>% 
  unlist() 

fit_df <- data.frame(x = fit_vector) %>% 
  mutate(type = "fit") %>% 
  bind_rows(data_df)
  

ggplot(data = fit_df, aes(x = x, y = ..density.., fill = type)) +
  geom_histogram(position = "dodge")
  
  

