#https://sourceforge.net/projects/mcmc-jags/files/
#testjags()
#install.packages("runjags")
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

jags.model.string <- "
model {
# priors
lambda ~ dgamma(0.005, 0.005) 
p ~ dunif(0,1)

# abundance component
for (i in 1:n.sites)
{
  N[i] ~ dpois(lambda) #Eq.1
  
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
totalN<-sum(N[])  #Estimate total population size across all sites
mean.N<- mean(N[])
fit<-sum(E[,])
fit.new<-sum(E.new[,])
} # close model loop
"

 # Parameters to monitor
params<-c("totalN", "mean.N", "lambda", "fit", "fit.new", "p", "N")

# jags data
jags.data <- list(Y = DATA, n.sites = NROW(DATA), n.surveys = NCOL(DATA)) 

# initial values
N.init <- DATA # initial count values 
N.init[is.na(N.init)] <- 1 # clean up NA's
N.init <- apply(N.init, 1, max) + 1 # zero values cause trouble
inits <- function() list(N = N.init)

# set run parameters
nc <- 3; na <- 2000; nb <- 2000; ni <- 10000; nt <- 10

# run jags
out.jags.pois <- run.jags(model = jags.model.string, data = jags.data,
                       monitor = params, n.chains = nc, inits = inits,
                       burnin = nb, adapt = na, sample = ni, thin = nt,
                       modules = "glm on", method = "parallel")

######RESULTS######## R^=psrf=potential scale reduction factor convergence statistic that should be close to 0 (<1.05)
SumStatPois<-as.data.frame(round(summary(out.jags.pois), 3)[ , c(1:5, 9, 11)])
SumStatPois
#plots the summary plots for each monitored parameter, trace, ecdf, posterior historgram & autocor + the autocorr 
#plot(out.jags.pois)
#Specify plots for a parameter, choices are "trace", "ecdf", "histogram", "autocorr", "key", "crosscorr"
#plot(out.jags.pois,plot.type=c("histogram", "trace"), vars="lambda",)

#Posterior Plots
data_vector = c(DATA)
data_df<-data.frame(x = data_vector[!is.na(data_vector)]) %>% 
     mutate(type = "Data")

MCMC<-combine.mcmc(out.jags.pois$mcmc) #combine.mcmc from runjags package

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
fit_vector_pN<-fit_vector_p * fit_vector_N


fit_df_N <- data.frame(x = fit_vector_N) %>% 
     mutate(type = "N")
fit_df_lambda<- data.frame(x = fit_vector_lambda) %>% 
     mutate(type = "lambda")
fit_df_p <- data.frame(x = fit_vector_p) %>% 
     mutate(type = "p")
fit_df_pN <- data.frame(x = fit_vector_pN) %>% 
     mutate(type = "Poisson Fit")

#####FIGURE 3A#####
fit_df<-bind_rows(data_df)

Count_Distribution <-ggplot(fit_df, aes(x=x, y = ..density.., color = type, fill=type)) +
     stat_bin(position="identity",center=0,binwidth=1, alpha=.2) +
     #geom_density(alpha=0.2, trim=T, adjust=.5) +
     xlim(-1,125) +
     #ylim(0,200000)+
     labs(x="Counted Animals", y="Density", title="Distribution of Counts", color = "Parameter", fill = "Parameter") +
     theme(legend.position="none")

Count_Distribution + theme_bw() +
     theme(legend.position = "none") +
     theme(plot.title = element_text(size = 20)) +
     theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12)) +
     theme(axis.title = element_text(size = 16)) #Figure 3

fit_df<-bind_rows(data_df,
                  #fit_df_lambda
                  fit_df_pN
)

######FIGURE 3C#######
Figure <- ggplot(fit_df, aes(x=x, y = ..density.., color = type, fill = type)) +
     stat_bin(position="identity",center=0,binwidth=1, alpha=.2) +
     # geom_density(alpha=0.5, trim=T, adjust=0.5) +
     scale_colour_manual(values=c("Data" = "#f8766d", #www.statology.org/ggplot-default-colors/
                                  #"N" = "#00bfc4",
                                  "Poisson Fit" = "#00ba38"
                                  #"lambda" = "#7cae00"
                                  ),
                         aesthetics = c("color", "fill"),
                         breaks=unique(fit_df$type)#keeps only used factors in the legend
     )+                    
     xlim(-1,125) +
     # ylim(0,200000)+
     labs(x="Number of Animals", y="Density", title="Distribution of Data and Poisson Posterior Model Fit", color = "Dist", fill = "Dist")

Figure + theme_bw() + 
  theme(legend.position = c(0.90, 0.5),legend.text = element_text(size = 10),legend.title = element_text(size = 0)) + #Figure 3-7 Width 800 Height 500
  theme(plot.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12)) +
  theme(axis.title = element_text(size = 16))


#Plot the DATA
BOXPLOT <-boxplot(DATA,
        ylab="Count",
        xlab="Transect",
        main="Distribution of Sturgeon Counts",
        names= c("Pass 1", "Pass 2", "Pass 3"),
        col="#69b3a2")

#Plot the Goodness of Fit Test, should look like a normally distributed cloud about the 1:1 line.
#In this case the line and data cloud are VERY far appart.
FIT<-c(unlist(out.jags.pois$mcmc[1][,"fit"]),unlist(out.jags.pois$mcmc[2][,"fit"]),unlist(out.jags.pois$mcmc[3][,"fit"]))
FITNEW<-c(unlist(out.jags.pois$mcmc[1][,"fit.new"]),unlist(out.jags.pois$mcmc[2][,"fit.new"]),unlist(out.jags.pois$mcmc[3][,"fit.new"]))
plot(FIT, FITNEW, 
     main="",
     xlim=c(0,150),
     xlab="Discrepancy actual data",
     ylab="Discrepancy replicate data"
)
abline(0,1,lwd=2,col="red")
