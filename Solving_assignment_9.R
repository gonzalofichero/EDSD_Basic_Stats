###########################################
# Exercise 2: Normal Distribution
###########################################

# Loading neccesary libraries
library(tidyverse)


# Assume that the amount of liquid filled in containers of a nominal value of 500 ml varies according a
# Normal distribution N($mu$ = 500; $sigma$ = 3).


# 1) How do you interpret the value of $mu$ = 500ml in this context?

# On average, the containers will be filled with 500ml. And we know that 95.45% of all containers
# will have between 494 and 506 ml (2 sigma from mean)


# 2) What is the probability that the volume filled in a container will fall below its nominal value by 1% or more?
pnorm(500*0.99, mean=500, sd=3)


# 3) Within which interval will the filling quantity of 99% of the containers lie?
qnorm(0.995, mean = 500, sd = 3)
qnorm(0.005, mean = 500, sd = 3)


# 4) The drug administration requests that in less than 10% cases the filling quantity deviates by more
# than 0.5% from the nominal value. Does the filling process described here meet this requirement?

1 - pnorm(500*1.005, mean=500, sd=3)
  # 20% higher than 10% limit


# 5) If the process does not meet the requirement, for which precision ($sigma$) of the process could the
# requirement be met?

# Graphic approach
sigma_seq <- seq(0.1,5, by=0.1)
x <- seq(450, 550, by=0.1)

plot(sigma_seq, 1 - pnorm(500*1.005, mean=500, sd=sigma_seq), 
     type="l", col="black", lwd=2, xlab="sigma", ylab="P(X>502.5)")
abline(h=0.1, col="red", lty=2, lwd=1.5)
locator()
# around 1.95



# Optimization approach (more elegant)

solving_sigma <- function(possible_sigmas){
  
  # Finding the P(X > 502.5) for different sigmas
  prob.dist <- (1 - pnorm(502.5, mean=500, sd=possible_sigmas))
  
  # We want to find the sigma where previuos probability is closer to 10%
  objective <- abs(prob.dist - 0.1)
  
  return(objective) }

# We optimize, finding minimum, for every possible value of sigma in (0,5) interval
ex9_optim <- optimize(solving_sigma, interval=c(0,5), maximum = FALSE)

# As in the graphic solution, the sigma that makes P(X > 502.5) < 10% is around 1.95
# more precisely: 1.95074
ex9_optim


# Checking:
1 - pnorm(500*1.005, mean=500, sd=1.95074)
# just below 10% :D


