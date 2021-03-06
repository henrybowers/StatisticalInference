# Statistical Inferencing - QUIZ 2

setwd('~/Projects/StatisticalInferencing/StatisticalInference')

#Q1 
#sigma^2/n

#Q2
# Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed with a mean of 80 (mm Hg) and a standard deviation of 10. About what is the probability that a random 35-44 year old has a DBP less than 70?

pnorm(70,80,10)

#Q3
#  Brain volume for adult women is normally distributed with a mean of about 1,100 cc for women with a
# standard deviation of 75 cc. About what brain volume represents the 95th percentile?

qnorm(0.95,1100,75)

#Q4

# Refer to the previous question. Brain volume for adult women is about 1,100 cc for women with a standard
# deviation of 75 cc. Consider the sample mean of 100 random adult women from this population. Around what
# is the 95th percentile of the distribution of that sample mean?

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




#Q7
0.5

#Q8
# The number of people showing up at a bus stop is assumed to be Poisson with a mean of 5 people per hour. You watch the bus stop for 3 hours. About what's the probability of viewing 10 or fewer people?

mean <- 5
time <- 3

lambda <- mean*time 

ppois(10, lambda=lambda)
