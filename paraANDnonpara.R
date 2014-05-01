#####################  HW#3  ##################################################
# Matthew Murray
# altered HW3.r file 
# File Name: HW3.R
# Purpose: Compare parameteric and Nonparameteric Confidence Intervals 
#          for Quantiles in HW 3
###############################################################################

#problem #2 (original code used for problem #1)
##############################################################################
mydata = c(10, 11, 18, 20, 5, 6, 8, 10, 9, 15)
n = length(mydata)


#### First calculate the parametric CI

# Calculate the mean of the sample
xbar = mean(mydata)
# Calculate the standard deviation of the sample
s = sd(mydata)
# Calculate the 50th percentile (the median) of the sample
p = 0.50
perc50 = xbar + s * qnorm(p)
# Calculate a two sided 90% confidence limit for the median
alpha = 0.10
LCL = xbar + (s/sqrt(n)) * qt(p=alpha/2, df=(n-1), ncp=(qnorm(p)*sqrt(n)))
UCL = xbar + (s/sqrt(n)) * qt(p=1-alpha/2, df=(n-1), ncp=(qnorm(p)*sqrt(n)))

# Print the results
perc50
LCL
UCL


#### Second calculate the nonparametric CI
mydata = sort(mydata)
p = 0.50

result = matrix(data=0, nrow=choose(n,2), ncol=3);
dimnames(result) = list(NULL, c("r","s","Confidence Level"));
i=1;
for(r in 1:(n-1))
{
  for(s in (r+1):n)
  {
    result[i,1] = r;
    result[i,2] = s;
    result[i,3] = pbinom(s-1, n, p) - pbinom(r-1, n, p);
    i = i + 1;
  }
}
sort(result[,3], index.return = T)

# When r = 3 and s = 8 the CL is 0.890625 which is approximately our goal 0.90
result[22,]
mydata[c(3, 8)]

# When r = 2 and s = 10 the CL is 0.9345703 which is 3% more than our goal 0.90
result[15,]
mydata[c(2, 8)]

#problem #3
##############################################################################
mydata = c(10, 11, 18, 20, 5, 6, 8, 10, 9, 15)
n = length(mydata)


#### First calculate the parametric CI

# Calculate the mean of the sample
xbar = mean(mydata)
# Calculate the standard deviation of the sample
s = sd(mydata)
# Calculate the 50th percentile (the median) of the sample
p = 0.75
perc75 = xbar + s * qnorm(p)
# Calculate a two sided 90% confidence limit for the median
alpha = 0.10
LCL = xbar + (s/sqrt(n)) * qt(p=alpha/2, df=(n-1), ncp=(qnorm(p)*sqrt(n)))
UCL = xbar + (s/sqrt(n)) * qt(p=1-alpha/2, df=(n-1), ncp=(qnorm(p)*sqrt(n)))

# Print the results
perc75
LCL
UCL


#### Second calculate the nonparametric CI
mydata = sort(mydata)
p = 0.75

result = matrix(data=0, nrow=choose(n,2), ncol=3);
dimnames(result) = list(NULL, c("r","s","Confidence Level"));
i=1;
for(r in 1:(n-1))
{
  for(s in (r+1):n)
  {
    result[i,1] = r;
    result[i,2] = s;
    result[i,3] = pbinom(s-1, n, p) - pbinom(r-1, n, p);
    i = i + 1;
  }
}
sort(result[,3], index.return = T)

# When r = 6 and s = 10 the CL is 0.8655596 which 3.5% less than our goal 0.90
result[39,]
mydata[c(6, 10)]

# When r = 5 and s = 10 the CL is 0.9239588 which is 2.3% more than our goal 0.90
result[35,]
mydata[c(5, 10)]

