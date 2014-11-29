## Quiz 4 - Statistical Inferencing

### Question 1
# A pharmaceutical company is interested in testing a potential blood pressure lowering medication. Their first examination 
# considers only subjects that received the medication at baseline then two weeks later. The data are as follows (SBP in mmHg)

#Subject  Baseline	Week 2
#1	140	132
#2	138	135
#3	150	151
#4	148	146
#	135	130

# Consider testing the hypothesis that there was a mean reduction in blood pressure? Give the P-value for the associated two 
# sided T test.

# asnwer 0.087


base <- (c(140,138,150,148,135))
final <- c(132,135,151,146,130)
t.test(base,final,alternative="two.sided",paired=TRUE)


### Question 2
# A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is the complete set 
# of values of ??actual mean that a test of H0:??=??0 would fail to reject the null hypothesis in a two sided 5% Students t-test?

# answer 1077 to 1123

meanS <- 1100
sdS <- 30
n <- 9
ul <- meanS + qt(0.975,n-1) * sdS/sqrt(n)
ll <- meanS - qt(0.975,n-1) * sdS/sqrt(n)
ul
ll

### Question 3
# Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people was asked which of two blinded drinks 
# given in random order that they preferred. The data was such that 3 of the 4 people chose Coke. Assuming that this sample is 
# representative, report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

# http://www.instantr.com/2012/11/06/performing-a-binomial-test/

# answer 0.31

binom.test(3,4,0.5,alternative="greater")

### Question 4
# Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high and are used as a 
# benchmark. A hospital that had previously been above the benchmark recently had 10 infections over the last 1,787 person days 
# at risk. About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard?

lambdaH0 <- 1/100
lambdaHA <- 10/1787
days <- 1787
infects <- 10
ppois(infects,lambdaH0*days,lower.tail=TRUE)

# ANSWER = 0.03

### Question 5
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects??? body mass indices (BMIs) 
# were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference 
# from follow-up to the baseline (followup - baseline) was ???3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. 
# The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo 
# group. Does the change in BMI appear to differ between the treated and placebo groups? Assuming normality of the underlying 
# data and a common population variance, give a pvalue for a two sided t test.


md <- -3 - 1
n1 <- n2 <- 9
var1 <- 1.5*sqrt(n1)
var2 <- 1.8*sqrt(n2)
sp <- sqrt(((n1-1)*var1 + (n2-1)*var2)/(n1 + n2-2))
semd <- sp * sqrt(1/n1 + 1/n2)

md + c(-1,1) * qt(0.95,n1 + n2 - 2) * semd

# T interval: -5.324521 -2.675479

# P Value for independent group T test (two-sided)
mean.diff <- md
df <- n1 + n2 - 2
sd1 <- 1.5
sd2 <- 1.8
pooled.var = (sd1^2 * n1 + sd2^2 * n2) / df
se.diff = sqrt(pooled.var/n1 + pooled.var/n2)
t.obt = mean.diff / se.diff
t.obt
p.value = 2*pt(t.obt,df=df)          # two-tailed
p.value

#ANSWER = Less than 0.01


### Question 6
# Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 1,123 cc. Would you reject in a two sided 5%
# hypothesis test of H0:??=1,078?

# ANSWER = Not Reject


### Question 7
# Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume loss of .01 mm3. 
# Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the power of 
# the study for a 5% one sided test versus a null hypothesis of no volume loss?
power.t.test(n=100,delta=0.01,sd=0.04,alternative="one.sided",type="one.sample",sig.level=0.05)$power

#ANSWER = 0.8


### Question 8
# Researchers would like to conduct a study of n healthy adults to detect a four year mean brain volume loss of .01 mm3. 
# Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the value of 
# n needded for 90% power of type one error rate of 5% one sided test versus a null hypothesis of no volume loss?
power.t.test(power=0.9,delta=0.01,sd=0.04,alternative="one.sided",type="one.sample",sig.level=0.05)$n

# ANSWER = 140


### Question 9
# As you increase the type one error rate, ??, what happens to power?

# ANSWER = You will get larger power
