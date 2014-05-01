#-----------------------------### HW 1 ###-------------------------------------#
# Matt Murray
# 2/5/14
# Stat 414
# Demonstrating the Central Limit Theorem by estimating the sampling 
# distribution of 1/xbar under a normal population.
#------------------------------------------------------------------------------#

# Clear all
rm(list = ls())

set.seed(01282009)

MC <- 10000 #Number of samples to simulate

sampling.tau <- function(mu, sigma, sampleSize, MC){
  tau_hat = c(1:MC)
  for(i in 1:MC)
  {
    mySample <- rnorm(n=sampleSize, mean=mu, sd=sigma)
    tau_hat[i] <- 1/mean(mySample)
  }
  return(tau_hat)
}

# mu=0.1, sigma=1, n= {10,20,30,100}
tau_hat11 <- sampling.tau(mu = 0.1, sigma = 1, sampleSize = 10, MC = MC)
tau_hat12 <- sampling.tau(mu = 0.1, sigma = 1, sampleSize = 20, MC = MC)
tau_hat13 <- sampling.tau(mu = 0.1, sigma = 1, sampleSize = 30, MC = MC)
tau_hat14 <- sampling.tau(mu = 0.1, sigma = 1, sampleSize = 100, MC = MC)
# mu=0.5, sigma=1, n= {10,20,30,100}
tau_hat21 <- sampling.tau(mu = 0.5, sigma = 1, sampleSize = 10, MC = MC)
tau_hat22 <- sampling.tau(mu = 0.5, sigma = 1, sampleSize = 20, MC = MC)
tau_hat23 <- sampling.tau(mu = 0.5, sigma = 1, sampleSize = 30, MC = MC)
tau_hat24 <- sampling.tau(mu = 0.5, sigma = 1, sampleSize = 100, MC = MC)
# mu=1, sigma=1, n= {10,20,30,100}
tau_hat31 <- sampling.tau(mu = 1, sigma = 1, sampleSize = 10, MC = MC)
tau_hat32 <- sampling.tau(mu = 1, sigma = 1, sampleSize = 20, MC = MC)
tau_hat33 <- sampling.tau(mu = 1, sigma = 1, sampleSize = 30, MC = MC)
tau_hat34 <- sampling.tau(mu = 1, sigma = 1, sampleSize = 100, MC = MC)
# mu=2, sigma=1, n= {10,20,30,100}
tau_hat41 <- sampling.tau(mu = 2, sigma = 1, sampleSize = 10, MC = MC)
tau_hat42 <- sampling.tau(mu = 2, sigma = 1, sampleSize = 20, MC = MC)
tau_hat43 <- sampling.tau(mu = 2, sigma = 1, sampleSize = 30, MC = MC)
tau_hat44 <- sampling.tau(mu = 2, sigma = 1, sampleSize = 100, MC = MC)

# Create histograms
par(mfrow=c(4,4))

hist(tau_hat11, main="mu=0.1, sigma=1, n=10")
hist(tau_hat12, main="mu=0.1, sigma=1, n=20")
hist(tau_hat13, main="mu=0.1, sigma=1, n=30")
hist(tau_hat14, main="mu=0.1, sigma=1, n=100")

hist(tau_hat21, main="mu=0.5, sigma=1, n=10")
hist(tau_hat22, main="mu=0.5, sigma=1, n=20")
hist(tau_hat23, main="mu=0.5, sigma=1, n=30")
hist(tau_hat24, main="mu=0.5, sigma=1, n=100")

hist(tau_hat31, main="mu=1, sigma=1, n=10")
hist(tau_hat32, main="mu=1, sigma=1, n=20")
hist(tau_hat33, main="mu=1, sigma=1, n=30")
hist(tau_hat34, main="mu=1, sigma=1, n=100")

hist(tau_hat41, main="mu=2, sigma=1, n=10")
hist(tau_hat42, main="mu=2, sigma=1, n=20")
hist(tau_hat43, main="mu=2, sigma=1, n=30")
hist(tau_hat44, main="mu=2, sigma=1, n=100")

