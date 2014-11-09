# Statistical Inferencing - QUIZ 1

setwd('~/R-projects/StatisticalInference')

#Q1 

atleastone <- 0.17
both <- 0.06
father <- 0.12

# P(mother or father) = P(mother) + P(father) - P(mother and father)
# P(mother)= P(mother or father) - P(father) + P(mother and father)
mother <- atleastone - father + both

#Q2
# 75th percentile of uniform PDF 1x1 box
l <- 0.75
w <- 1
a75th <- l*w

#Q3

# P(heads) = p
# P(tails) = 1-p

# 0 = p*X - (1-p)*Y
# p*X = (1-p)*Y
# p*X/Y= (1-p)
# X/Y = (1-p)/p


#Q4

#median of PDF symmetric about 0 = 0

#Q5

x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp
mean <- sum(x*p)


#Q6

