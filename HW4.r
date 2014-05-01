# HW4.r
# Matt Murray
# Stat 414
# Created 3/5/14
# Comparing 95% CI coverage while bootstrapping over a random 
# sample from a lognormal distribution
##############################################################

# Clear all
rm(list = ls())

#Distribution Parameters
n <- 10;  #run for values of 10,25,50
mu <- 1;  # run for values 1 and 2
sigma <- 1.5;  #run for values 1.5 and .2

B <- 1000; # Number of samples to simulate
result <- vector();
xbar_star = c(1:B)
the_mean = exp(mu+sigma^2/2);
alpha <- 0.05
log_z<-qlnorm(1-alpha/2)

#run bootstrap for 95% CI, with B=1000, 1000 times
for(i in 1:B)
{
  #create random sample
  mySample <- rlnorm(n=n, meanlog=mu, sdlog=sigma);
  xbar = mean(mySample)
  #create botstrapped sample of B=1000
  for(b in 1:B){
    x_star = sample(x=mySample, size=n, replace=TRUE)
    xbar_star[b] = mean(x_star)
  }
  #determine 95% confidence interval
  lowerCL <- mean(mySample)-log_z*sd(mySample)/sqrt(B);
  upperCL <- mean(mySample)+log_z*sd(mySample)/sqrt(B);
  #does the interval contain the population mean
  result[i] <- ((lowerCL < the_mean) & (the_mean < upperCL));
}
#get percentage of 
SimulatedConfidenceLevel<- mean(result)*100;

SimulatedConfidenceLevel
