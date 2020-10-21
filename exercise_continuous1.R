###################################################
# Exercises: Continuous Distributions, Part I
# Exercise 1
# Suppose the life spans (in years) of some animal species follow an Exponential distribution with lambda = 0:25.

lambda <- 0.25

# A)
pexp(4, rate = lambda) - pexp(3, rate = lambda)

# B)
1/lambda

# C)
qexp(0.99, rate = lambda, lower.tail = TRUE, log.p = FALSE)

# D)
qexp(0.5, rate = lambda, lower.tail = TRUE, log.p = FALSE)
# median = ln 2 / lambda

# E)

pi <- seq(0,50,length=1001)


plot(pi, pexp(pi, rate = lambda), type="l", col="blue", lwd=2, xlab="Life Span (years)", ylab="cdf")
abline(v=log(2)/lambda, col="red", lty=2, lwd=1)
abline(h=0.5, col="red", lty=2, lwd=1)




###################################################
# Exercises: Continuous Distributions, Part I
# Exercise 2
# Applicants for a parking permit are assigned a 15 minutes interval at the local municipal office (e.g. 8:00-
# 8:15). The arrival time during this 15-minutes-slot can be modeled by a uniform distribution. (We also
# can assume that all applicants will make use of their allotted time slot.)

pi_uni <- seq(0,15,length=101)

# A)
plot(pi_uni, dunif(pi_uni, min = 0, max = 15, log = FALSE), type="l", col="blue", lwd=2, xlab="Arrival time (minutes)", ylab="density")
plot(pi_uni, punif(pi_uni, min = 0, max = 15, log = FALSE), type="l", col="blue", lwd=2, xlab="Arrival time (minutes)", ylab="cdf")


# B)
punif(5, min = 0, max = 15, log = FALSE)

# C) mean = (b-a)/2 => 7.5 minutes

# D)
qunif(0.75, min = 0, max = 15, log = FALSE)

# E) 
(punif(7, min = 0, max = 15, log = FALSE) - punif(5, min = 0, max = 15, log = FALSE)) / (1 - punif(5, min = 0, max = 15, log = FALSE))
(punif(12, min = 0, max = 15, log = FALSE) - punif(10, min = 0, max = 15, log = FALSE)) / (1 - punif(10, min = 0, max = 15, log = FALSE))
# the second is 2 times the first one


###################################################
# Exercises: Continuous Distributions, Part I
# Exercise 3
# A customer wants to make a reservation at a restaurant over the phone. The time (in minutes) until the
# phone is answered in the restaurant can by modeled by an Exponential distribution with lambda = 4/3


# A) mean = 1 / lambda => 0.75 minutes

# B) 
1 - pexp(1, rate = 4/3)

# C) Y ~ Geometric distribution
      # with pi = pexp(0.5, rate = 4/3, lower.tail = TRUE, log.p = FALSE) = 0.4865829
      # with mean = 2.055148

# D)
test.lambda <- seq(1,2, by=0.1)

prob_success <- pexp(0.5, rate = test.lambda, lower.tail = TRUE, log.p = FALSE)

prob_half_minute <- pgeom(1, prob_success, log=FALSE)

plot(test.lambda, prob_half_minute, type="l", col="blue", lwd=2, xlab="Lambda of time (in minutes) until the phone is answered", ylab="Prob at most 2 calls from customer")
abline(h=0.8, col="red", lty=2, lwd=1)



###################################################
# Exercises: Continuous Distributions, Part I
# Exercise 4
# An engineer has constructed a new device. The time (in years) until such a 
# device fails can be assumed to follow an exponential distribution with parameter lambda = 0.12

# A) mean = 1/lambda
1 / 0.12

# B) 
1 - pexp(2, rate = 0.12)

# C)
qexp(0.25, rate = 0.12)

#D) 
lambda.device <- seq(0.4,0.6, by=0.01)
prob.3.years <- pexp(3, rate = lambda.device)

plot(lambda.device, prob.3.years, type="l", col="blue", lwd=2, xlab="Lambda of time (in years) until device fails", ylab="Prob device will last at least 3 years")
abline(h=0.75, col="red", lty=2, lwd=1)

# E + F)  Y ~ Binomial distribution
                # with pi = pexp(2, rate = 0.12) = 0.2133721
                # n = 100
                # with mean = 21.33721 claims





