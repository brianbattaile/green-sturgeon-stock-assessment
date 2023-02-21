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
Data1<-Count2020
Data2<-Data1[!apply(Data1, 1, function(Data1) all(is.na(Data1))),]#remove rows with all NA
Data3<-Data2[,colSums(is.na(Data2))<nrow(Data2)]#remove colunms with all NA
DATA = Data3

jags.model.string <-" 
model {
# priors
# detection (hyper) prior
      #Changed to mu in paper to avoid confusion with Betas in Covariate models.
     beta~dunif(0,1) #dnorm(0,1) #Eq.18
# Abundance prior
     alpha.lam~dnorm(0,1) #Eq.14
# Abundance noise
     for (i in 1:n.sites){
          eps[i]~dnorm(0, tau.lam) #Eq.11
     }
     tau.lam<-1/(sd.lam*sd.lam) #Eq.12
     sd.lam~dunif(0,5) #Eq.13
#Detection noise
     tau.p<-1/(sd.p*sd.p) #Eq.19
     sd.p~dunif(0,3) #Eq.20
     
#Abundance component
for (i in 1:n.sites)
{
     N[i] ~ dpois(lambda[i]) # Eq.9 poisson
     log(lambda[i])<-alpha.lam+eps[i] #Eq.10
# detection component
     for (j in 1:n.surveys)#n.surveys numsur[i]
     {
          Y[i, j] ~ dbin(p[i,j], N[i]) #Eq.2
          p[i,j]<-exp(lp[i,j])/(1+exp(lp[i,j])) #Eq.16 logit back transform
          lp[i,j]~dnorm(beta, tau.p) #Eq.17
          
     #Assess model goodness of fit
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
          "beta", "tau.p", "mean.detection", "sd.p","fit", "fit.new",
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
out.jags.BpoisOD <- run.jags(model = jags.model.string, data = jags.data,
                            monitor = params, n.chains = nc, inits = inits,
                            burnin = nb, adapt = na, sample = ni, thin = nt,
                            modules = "glm on", method = "parallel")
     
######RESULTS######## R^=psrf=potential scale reduction factor convergence statistic that should be close to 0 (<1.05)
SumStatBPoisOD<-as.data.frame(round(summary(out.jags.BpoisOD), 3)[ , c(1:5, 9, 11)]) 
SumStatBPoisOD
#plots the summary plots for each monitored parameter, trace, ecdf, posterior historgram & autocor + the autocorr 
#plot(out.jags.BpoisOD)
#Specify plots for a parameter, choices are "trace", "density", ecdf", "histogram", "autocorr", "key", "crosscorr"
#plot(out.jags.BpoisOD,plot.type=c("histogram", "trace"), vars="beta",)

#Plot the Goodness of Fit Test, should look like a normally distributed cloud about the 1:1 line
FIT<-c(unlist(out.jags.BpoisOD$mcmc[1][,"fit"]),unlist(out.jags.BpoisOD$mcmc[2][,"fit"]),unlist(out.jags.BpoisOD$mcmc[3][,"fit"]))
FITNEW<-c(unlist(out.jags.BpoisOD$mcmc[1][,"fit.new"]),unlist(out.jags.BpoisOD$mcmc[2][,"fit.new"]),unlist(out.jags.BpoisOD$mcmc[3][,"fit.new"]))
plot(FIT, FITNEW, 
     main="", 
     xlab="Discrepancy actual data",
     ylab="Discrepancy replicate data",
     frame.plot=FALSE
)
abline(0,1,lwd=2,col="red")

###GRAPHING THE POSTERIOR MCMC###   
###THIS IS FIGURE 7#####   
data_vector = c(DATA)
data_df<-data.frame(x = data_vector[!is.na(data_vector)]) %>% 
        mutate(type = "Data")
     
MCMC<-combine.mcmc(out.jags.BpoisOD$mcmc) #combine.mcmc from runjags package
     
fit_vector_N<-data.frame(MCMC) %>%
        select(starts_with("N")) %>% 
        as.matrix() %>% 
        c() %>%
        unlist()
fit_vector_lambda<-data.frame(MCMC) %>%
        select(starts_with("l")) %>% 
        as.matrix() %>% 
        c() %>%
        unlist()
fit_vector_p<-data.frame(MCMC) %>%
        select(starts_with("p")) %>% 
        as.matrix() %>% 
        c() %>%
        unlist()
fit_vector_p1N<-fit_vector_p[1:ni*nc*NROW(DATA)] * fit_vector_N
fit_vector_p2N<-fit_vector_p[ni*nc*NROW(DATA)+1:ni*nc*NROW(DATA)*2] * fit_vector_N
fit_vector_p3N<-fit_vector_p[ni*nc*NROW(DATA)*2+1:ni*nc*NROW(DATA)*3] * fit_vector_N
     
fit_vector_pN<-c(fit_vector_p1N,fit_vector_p2N,fit_vector_p3N)
     
fit_df_N <- data.frame(x = fit_vector_N) %>% 
        mutate(type = "N")
fit_df_lambda<- data.frame(x = fit_vector_lambda) %>% 
        mutate(type = "lambda")
fit_df_p <- data.frame(x = fit_vector_p) %>% 
        mutate(type = "p")
fit_df_pN <- data.frame(x = fit_vector_pN) %>% 
        mutate(type = "OD Poisson Fit")
     
fit_df<-bind_rows(data_df,
                  fit_df_pN
)
Figure<-ggplot(fit_df, aes(x=x, y = ..density.., fill = type, col = type)) +
        stat_bin(position="identity",center=0,binwidth=1, alpha=.2) +  #alpha=.5 and col="black" for different look
        # geom_density(alpha=0.5, trim=T, adjust=0.5) +
        scale_colour_manual(values=c("Data" = "#f8766d", #www.statology.org/ggplot-default-colors/
                                     #"N" = "#00bfc4",
                                     "OD Poisson Fit" = "#c77cff"
                                     #"lambda" = "#7cae00"
                                     ),
                            aesthetics = c("color", "fill"),
                            breaks=unique(fit_df$type)#keeps only used factors in the legend
        )+                    
        xlim(-1,125) +
        # ylim(0,200000)+
        labs(x="Number of Animals", y="Density", title="Overdispersed Poisson Posterior Model Fit", color = "Dist", fill = "Dist")
Figure + theme_bw() + 
  theme(legend.position = c(0.90, 0.5),legend.text = element_text(size = 10),legend.title = element_text(size = 0)) + #Figure 3 and 4 Width 800 Height 500
  theme(plot.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12)) +
  theme(axis.title = element_text(size = 16)) #Figure 3
     