library(tidyverse)
library(ggplot2)

###############################################################################################
# Exercise 1
# The file flyages.txt contains a subsample of a larger experiment (original paper is in the
# cloud). It contains ages at death (in days) of a species of fruit flies

# Importing the data
flies <- read.delim("C:/Users/Gonzalo/Desktop/EDSD/01 - Rostock/EDSD 130 Basic Stats/Exercises/flyages.txt", header = TRUE)

# Looking the data
glimpse(flies)


# Loglikelihood for Weibull
logL.Weibull <- function(para, x){
  a <- para[1]
  b <- para[2]
  
  logL <- sum( log(a) - log(b) + (a-1) * log(x/b) - ((x/b)^(a)) )  
  
  return(logL)
}

# 1. Plot a histogram for these data
hist(flies$x)

# 2. Estimate a Weibull distribution for these data by maximum likelihood. Give 95% confidence
# intervals for the parameters a and b. Plot the density and compare with the histogram

p.start <- c(2,5)

mle.weibull <- optim(p.start, logL.Weibull, x=flies$x, control=list(fnscale=-1), hessian=TRUE )

mle.weibull

# extract the MLEs for a and b
a.hat <- mle.weibull$par[1]
b.hat <- mle.weibull$par[2]

# Build variance-covariance from Hessian 
V <- solve(-mle.weibull$hessian)      # inverse of negative Hessian
V
#  square root of diagonal elements are the s.e. of a and b
sqrt(diag(V))
# parameter estimates correlated?
cov2cor(V)

# CI for a and b (assuming 1-alpha=95%)
se.a <- sqrt(diag(V)) [1]
se.b <- sqrt(diag(V)) [2]

c(a.hat - 1.96*se.a , a.hat + 1.96*se.a) 
c(b.hat - 1.96*se.b , b.hat + 1.96*se.b) 


# Plotting real data vs data from MLE
days <- seq(1, max(flies$x), by = 1)
weibull.mle.data <- dweibull(days, shape=a.hat, scale=b.hat) #* 1000 # to match frequencies and not densities

hist(flies$x)
lines(days, weibull.mle.data, type="l", col="red", lwd=3)

plot(density(flies$x))
lines(days, weibull.mle.data, type="l", col="red", lwd=3)


# 3. Estimate the median lifetime from the Weibull distribution and give a 95% confidence interval

q.hat <- b.hat * C

C <- ((-log(0.5))^(1/a.hat))

# now gradient
g.a <- (-1) * C / (a.hat*a.hat*log(0.5))
g.b <- C

gradient <- c(g.a, g.b)

se.q <- sqrt( t(gradient) %*% V %*% gradient)
se.q      # is 1x1 

#    confidence interval for q 
c(q.hat - 1.96 * se.q , q.hat + 1.96 * se.q)




###############################################################################################
# Exercise 2
# In Exercise 1 of Likelihood Continuous Distributions we estimated a Gompertz distribution from a sample
# of 400 individuals. Use the same data here.


# 1. Give 95% confidence intervals for the parameter estimates a and b, respectively.

# Bring back from Ex 1 of LogLikehood module
optimal.gompertz

# extract the MLEs for a and b
a.hat <- optimal.gompertz$par[1]
b.hat <- optimal.gompertz$par[2]

# Build variance-covariance from Hessian 
V <- solve(-optimal.gompertz$hessian)      # inverse of negative Hessian
V
#  square root of diagonal elements are the s.e. of a and b
sqrt(diag(V))


# CI for a and b (assuming 1-alpha=95%)
se.a <- sqrt(diag(V)) [1]
se.b <- sqrt(diag(V)) [2]

c(a.hat - 1.96*se.a , a.hat + 1.96*se.a) 
c(b.hat - 1.96*se.b , b.hat + 1.96*se.b) 


# 2. Can you estimate the correlation between the two parameter estimates?
cov2cor(V)
# Yes, negative correlated -0.6


# 3. The mode of a distribution is the value where the density has its highest value. It marks the age at
# death (in this example) with the highest probability.
# What is the estimated modal age at death from the sample? Give a 95% confidence interval for
# this estimate.

# For Gompertz, the modal age at death (M) = exp(ln(b/a)/b+1)

m.hat <- exp(log(b.hat/a.hat)/(b.hat+1))


# now gradient
g.a <- m.hat * (1/(b.hat+1)) * (-1/a.hat)
g.b <- m.hat * (-log(a.hat)/((b.hat+1)^2)) * (((b.hat+1)/b.hat - log(b.hat))/((b.hat+1)^2))

gradient <- c(g.a, g.b)

se.q <- sqrt( t(gradient) %*% V %*% gradient)
se.q      # is 1x1 

#    confidence interval for M 
c(m.hat - 1.96 * se.q , m.hat + 1.96 * se.q)
m.hat




###############################################################################################
# Exercise 3
# The Logistic distribution is an alternative to the Normal distribution. Its density is bell-shaped around
# the mean, but it is slightly more concentrated around the center than the Normal

# Importing the data
blood <- read.delim("C:/Users/Gonzalo/Desktop/EDSD/01 - Rostock/EDSD 130 Basic Stats/Exercises/SBPindividual.txt", header = TRUE)

glimpse(blood)

hist(blood$x)

# Let's create the LL of the Logistic
logL.logistic <- function(para, x){
  m <- para[1]
  s <- para[2]
  
  logL <-   
  
  return(logL)
}




