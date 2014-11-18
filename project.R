# Statistical Inferencing Project - Nov 2014 - Henry Bowers

mns = NULL
for (i in 1 : 100000) mns = c(mns, mean(runif(100)))
hist(mns)
mean(mns)
var(mns)


#exponential mean = 1/lambda
#exponential standard deviation = 1/lambda
lambda <- 0.2
meanT <- 1/lambda
SDT <- 1/lambda

#simulate exponential
set.seed(1234)
mns <- NULL
n <- 1000
for (i in 1 : n) mns = c(mns, mean(rexp(40,lambda)))
hist(mns)
var(mns)
sd(mns)
