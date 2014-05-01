############################ HW #2 ###################################################
# Matthew Murray
# 2/12  
# Modified Week3Examples 
# Purpose: To discuss Confidence Interval Estimation and Quantiles
######################################################################################

# Question #1

#------------------------### EXAMPLE 1 ###-------------------------------------#
# Calculating z_p in a 90% CI
#------------------------------------------------------------------------------#
# Clear all
rm(list = ls())

alpha = 0.10
p = 1-alpha/2
z_p = qnorm(p)

#------------------------### EXAMPLE 2 ###-------------------------------------#
# Calculating t_p in a 90% CI
#------------------------------------------------------------------------------#
# Clear all
rm(list = ls())

alpha = 0.10
p = 1-alpha/2
v = 15
t_p = qt(p,v)

#------------------------### EXAMPLE 3 ###-------------------------------------#
# Output of an R program for computing the confidence level through simulation.
# The population here is normal. 
#------------------------------------------------------------------------------#

MC <- 10000 # Number of samples to simulate
result <- c(1:MC)
mu <- 50
sigma <- 4
n <- 8; # Sample size
alpha <- 0.10 # the nominal confidence level is 100(1-alpha) percent
t_criticalValue <- qt(p=(1-alpha/2), df=(n-1))

for(i in 1:MC){
  mySample <- rnorm(n=n, mean=mu, sd=sigma)
  lowerCL <- mean(mySample)-t_criticalValue*sd(mySample)/sqrt(n)
  upperCL <- mean(mySample)+t_criticalValue*sd(mySample)/sqrt(n)
  result[i] <- ((lowerCL < mu) & (mu < upperCL))
}
SimulatedConfidenceLevel <- mean(result)

#------------------------### EXAMPLE 4 ###-------------------------------------#
# Example on page 277 of Millard and Neerchal
#------------------------------------------------------------------------------#

# Define the variable which hold the data
Well_1 = c(19.9, 29.6, 18.7, 24.2)
n1 = length(Well_1)

Well_2 = c(23.7, 21.9, 26.9, 26.1)
n2 = length(Well_2)

Well_3 = c(25.6, 23.3, 22.3, 26.9)
n3 = length(Well_3)


# Calculate the mean of the sample in well 1
xbar1 = mean(Well_1)
# Calculate the standard deviation of the sample in well 1
s1 = sd(Well_1)
# Calculate the 95th percentile of the sample in well 1
perc95_1 = xbar1 + s1 * qnorm(.9)
# Calculate the Lower confidence limit of the sample in well 1
LCL1 = xbar1 + (s1/sqrt(n1)) * qt(p=.01, df=(n1-1), ncp=(qnorm(.9)*sqrt(n1)))


# Calculate the mean of the sample in well 2
xbar2 = mean(Well_2)
# Calculate the standard deviation of the sample in well 2
s2 = sd(Well_2)
# Calculate the 95th percentile of the sample in well 2
perc95_2 = xbar2 + s2 * qnorm(.9)
# Calculate the Lower confidence limit of the sample in well 2
LCL2 = xbar2 + (s2/sqrt(n2)) * qt(p=.01, df=(n2-1), ncp=(qnorm(.9)*sqrt(n2)))


# Calculate the mean of the sample in well 3
xbar3 = mean(Well_3)
# Calculate the standard deviation of the sample in well 3
s3 = sd(Well_3)
# Calculate the 95th percentile of the sample in well 3
perc95_3 = xbar3 + s3 * qnorm(.9)
# Calculate the Lower confidence limit of the sample in well 3
LCL3 = xbar3 + (s3/sqrt(n3)) * qt(p=.01, df=(n3-1), ncp=(qnorm(.9)*sqrt(n3)))

# Results for well 1 
xbar1
s1
perc95_1 
LCL1
# Results for well 2
xbar2
s2
perc95_2
LCL2
# Results for well 3
xbar3
s3
perc95_3
LCL3

#################################################################################################
# Question #2

rm(list = ls())

par(mfrow=c(2,1))

x <- c(1:30)
y1 <- dlnorm(x,meanlog=1,sdlog=1.5,log=FALSE)
y2 <- dlnorm(x,meanlog=2,sdlog=.2,log=FALSE)
plot(x,y1,type='l',,main ="mu= 1, sd= 1.5")
plot(x,y2,type='l',,main ="mu= 2, sd= .2")
#################################################################################################
# Question #3

rm(list = ls())
# mu = 1, sd = 1.5
#
MC <- 10000 # Number of samples to simulate
result <- c(1:MC)
mu <- 1
sigma <- 1.5
n <- 8 # Sample size
alpha <- 0.10 # the nominal confidence level is 100(1-alpha) percent
t_criticalValue <- qt(p=(1-alpha/2), df=(n-1))

for(i in 1:MC){
  mySample <- rnorm(n=n, mean=mu, sd=sigma)
  lowerCL <- mean(mySample)-t_criticalValue*sd(mySample)/sqrt(n)
  upperCL <- mean(mySample)+t_criticalValue*sd(mySample)/sqrt(n)
  result[i] <- ((exp(lowerCL) < mu) & (mu < exp(upperCL)))
}

SimulatedConfidenceLevel <- mean(result)
SimulatedConfidenceLevel
#################################################
# mu = 2 sd = .2
#
result2 <- c(1:MC)
mu2 <- 2
sigma2 <- .2
n2 <- 8 # Sample size
alpha2 <- 0.10 # the nominal confidence level is 100(1-alpha) percent
t_criticalValue2 <- qt(p=(1-alpha2/2), df=(n2-1))

for(i in 1:MC){
  mySample2 <- rnorm(n=n2, mean=mu2, sd=sigma2)
  lowerCL2 <- mean(mySample2)-t_criticalValue2*sd(mySample2)/sqrt(n2)
  upperCL2 <- mean(mySample2)+t_criticalValue2*sd(mySample2)/sqrt(n2)
  result2[i] <- ((exp(lowerCL2) < mu2) & (mu2 < exp(upperCL2)))
}

SimulatedConfidenceLevel2 <- mean(result2)
SimulatedConfidenceLevel2

