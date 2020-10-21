library(tidyverse)
library(ggplot2)



###################################################
# Exercises: Likelihood Continuous Distributions
# Exercise 1
# The Gompertz distribution can describe the ages at death for humans above the age of 50 very well

# A) Write a function for the log-likelihood of a Gompertz distribution that you can use for numerical optimization

gompertz.ll <- function(pars, x) {
  
  a <- pars[1]
  b <- pars[2]
  
  logL <-sum(log(a) + b*x- (a/b) *(exp(b*x) - 1))
  
  return(logL)
}

# Data for ages at death over 50 years old
age.death <- read.delim("C:/Users/Gonzalo/Desktop/EDSD/01 - Rostock/EDSD 130 Basic Stats/Exercises/ages-at-death.txt", header = TRUE)

# From data.frame to vector and standardized to 50 years old
#age.death.vector <- age.death %>% slice(1) %>% unlist() %>% unname()
age.death.vector.stand <- age.death$age - 50


# Calculating a and b from optimization
start.point.gompertz <- c(0.005,0.07)

optimal.gompertz <- optim(start.point.gompertz, fn=gompertz.ll, x=age.death.vector.stand, control=list(fnscale=-1), hessian = T, method="BFGS")
optimal.gompertz
# a = 76.875
# b = -4.625

# Histogram of data
age.death %>% 
  ggplot(aes(x=age)) + geom_histogram(binwidth = 2)

# Histogram from MLE Gomperzt model
ages <- seq(50,100, by = 1)
a.mle <- 76.875
b.mle <- -4.625

mle.gompertz <- log(a.mle) + b.mle*(ages-50) + (-a.mle/b.mle) * (exp(b.mle*(ages-50) -1))

plot(ages, mle.gompertz, type="l", col="blue", lwd=2, xlab="age", ylab="Gompertz P(x)")



# Checking f(x) for differente parameters
ages2 <- seq(1,100, by = 1)
gompertz1 <- 0.005 * exp(0.07*ages2) * exp(-(0.005/0.07)*(exp(0.07*ages2)-1))
gompertz2 <- 0.5 * exp(0.75*ages2) * exp(-(0.5/0.75)*(exp(0.75*ages2)-1))
gompertz3 <- 0.05 * exp(0.075*ages2) * exp(-(0.05/0.075)*(exp(0.075*ages2)-1))
gompertz4 <- 0.0005 * exp(0.007*ages2) * exp(-(0.0005/0.007)*(exp(0.007*ages2)-1))

plot(ages2, gompertz1, type="l", col="blue", lwd=2, xlab="age", ylab="Gompertz P(x)", ylim=c(0,0.5))
lines(ages2, gompertz2, type="l", col="red", lwd=2)
lines(ages2, gompertz3, type="l", col="black", lwd=2)
lines(ages2, gompertz4, type="l", col="green", lwd=2)







###################################################
# Exercise 2
# The life spans (in days) of a species of beetles can be modeled by an exponential distribution


# Creating a LogLikelihood function for Exponential exercise
# Lambda has to be optimized for MLE, and sample.exp is a vector
log.Lexp <- function(lambda, LR, CR){
  
  logL <-  LR*log(pexp(q=21,rate=lambda))+
    CR*log((1-pexp(q=21,rate=lambda)))
  
  return(logL)
}

sample.exp <- c(53,47)
start.point <- 0.5

#optimize(log.Lexp, counts=sample.exp, interval=c(0,2), maximum = TRUE)
optim(start.point, fn=log.Lexp, LR = sample.exp[1] , CR = sample.exp[2], control=list(fnscale=-1))
# lambda = 0.03596191 for 2 category sample for both algorithm
# mean = 1 / lambda = 27.8 (days?)


# But one day we found the non aggregated data
complete.data <- read.delim("C:/Users/Gonzalo/Desktop/EDSD/01 - Rostock/EDSD 130 Basic Stats/Exercises/beetlelife.txt", header = TRUE)

# From data.frame to vector
complete.data.vector <- complete.data %>% slice(1) %>% unlist() %>% unname()


# Now we can estimate the lambda for the new data

log.Lexp2 <- function(lambda, counts){
  
  logL <- sum(log(dexp(counts, rate=lambda)))
  
  return(logL)
}

optim(start.point, fn=log.Lexp2, counts=complete.data.vector, control=list(fnscale=-1))
# Now the lambda looks like 0.08334961 -> 2.5x bigger than previous calculation
# now the mean is 12 (days?)


