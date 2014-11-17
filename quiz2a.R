# Statistical Inferencing - QUIZ 2

setwd('~/Projects/StatisticalInferencing/StatisticalInference')

#Q1 
#sigma^2/n

#Q2
# Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed with a mean of 80 (mm Hg) and a standard deviation of 10. About what is the probability that a random 35-44 year old has a DBP less than 70?
16
pnorm(70,80,10)

#Q3
#  Brain volume for adult women is normally distributed with a mean of about 1,100 cc for women with a
# standard deviation of 75 cc. About what brain volume represents the 95th percentile?
1223
qnorm(0.95,1100,75)

#Q4

# Refer to the previous question. Brain volume for adult women is about 1,100 cc for women with a standard
# deviation of 75 cc. Consider the sample mean of 100 random adult women from this population. Around what
# is the 95th percentile of the distribution of that sample mean?
1112

mean <- 1100
n <- 100
psd <- 75
pvar <- psd^2
svar <- pvar/n
ssd <- sqrt(svar) 
qnorm(0.95,1100,ssd)


# va

#Q5
# flip a fair coin 5 times. P(4 or 5 heads)
0.19
# binomial distribution
# X = number of trials = 5 for this problem
# Y = number of times a trial outcome = 1 (or 0) = 4 or 5 for this problem
# choose(X,Y) * 0.5 ^ X , so for this problem it's 

choose(5,4) * 0.5 ^ 5 + choose(5,5) * 0.5 ^ 5

#or

pbinom(3, size = 5, prob = 0.5, lower.tail = FALSE)


#Q6
# The respiratory disturbance index (RDI), a measure of sleep disturbance, for a specific population has
# mean of 15 (sleep events per hour) and a standard deviation of 10. They are not normally distributed.
# Give your best estimate of the probability that a sample mean RDI of 100 people is between 14 and 16
# events per hour?
0.68

#install.packages('truncnorm')
library(truncnorm)
samples<-apply(matrix((rtruncnorm(100,a=0,mean=15,sd=10),0),100000),1,mean)
meanmean <- mean(samples)
vmean<-var(samples)
stmean<-sqrt(vmean)
table(samples)
hist(samples)

pnorm(16,meanmean,sqrt(vmean)) - pnorm(14,meanmean,sqrt(vmean))

# 50% < 15, 50% > 15
# so for sample size = 100, 50 are between 0 and 15, 50 are > 15
# for sample size 100, 68 will fall between 5 and 25

pmean <-  15
psd <- 10
pvar <- psd^2

pnorm(16,pmean,psd)-pnorm(14,pmean,psd)

#95% confidence interval
56/100 + 1/sqrt(100)
56/100 - 1/sqrt(100)



#CLT approach
samplestandarderror <- 

#chebyshev's inequality
# min 50% of samples must be between lower bound and upper bound
# mean - sqrt(2) * SD = lower bound 
# mean + sqrt(2) * SD = upper bound
  
# the number of standard deviations 14 is from the mean: 1/10 = 0.1
# Assymetric two-sided case
k1 <- 14
k2 <- 16
prob14and16 <-  4 * ((pmean - k1) * (k2 - pmean) - pvar) / (k2-k1)^2

#Q7
0.5

#Q8
# The number of people showing up at a bus stop is assumed to be Poisson with a mean of 5 people per hour. You watch the bus stop for 3 hours. About what's the probability of viewing 10 or fewer people?

mean <- 5
time <- 3

lambda <- mean*time 

ppois(10, lambda=lambda)
0.12