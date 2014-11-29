# Statistical Inferencing Project - Nov 2014 - Henry Bowers

setwd("~/R-projects/StatisticalInference/StatisticalInference")

#simulate dist of means for sample size = 40 of exp distribution

lambda<-0.2

meanDistExp <- function(reps,size) {
  dist <- NULL
  for (i in 1 : reps) dist = c(dist, mean(rexp(size,lambda))) 
  return(dist)
}

mns <- meanDistExp(1000,40)
simMean <- mean(mns)
simSD <- sd(mns)
par(500,500)
hist(mns)
abline(v=simMean, col="red", lwd=2)
text(simMean+2,225,paste("Sample Mean = ",round(simMean,2)))

# stats for exponential distribution with lambda = 2
lambda <- 0.2
popMean <- 1/lambda
popSD <- 1/lambda
popVar <- popSD^2

### Point 1 ###
paste("Simulated Mean = ", round(simMean,2))
paste("Theoretical Mean = ",popMean)

### Point 2 ###
paste("Simulated Standard Deviation = ",round(simSD,2))
paste("Theoretical Standard Deviation = ",popSD)

### Point 3 ### 

#compare density of left and right side of mean distribution

r <- NULL
for(n in c(40,100,1000)) {
  mns <- meanDistExp(1000,n)
  simMean <- mean(mns)
  diffMean <- mns-simMean
  r <- rbind(r, c(n,lessThanZeroPercent <- length(subset(diffMean, diffMean < 0))/length(diffMean), length(subset(diffMean, diffMean > 0))/length(diffMean)))
}
r


#### Part 2 ###
# Now in the second portion of the class, we're going to analyze the ToothGrowth data in the R datasets package. 
# Load the ToothGrowth data and perform some basic exploratory data analyses 
# Provide a basic summary of the data.
# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from
# class, even if there's other approaches worth considering)
# State your conclusions and the assumptions needed for your conclusions. 
# Some criteria that you will be evaluated on
#   Did you  perform an exploratory data analysis of at least a single plot or table highlighting basic features of the data?
#   Did the student perform some relevant confidence intervals and/or tests?
#   Were the results of the tests and/or intervals interpreted in the context of the problem correctly? 
#   Did the student describe the assumptions needed for their conclusions?

library(plyr)
library(ggplot2)

data(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
summary(ToothGrowth)

#plot dist of tooth length by supplement type for each dose level
par(500,500)
plot<-ggplot(ToothGrowth,aes(x=supp,y=len))
plot+geom_point(size=3,aes(color=supp))+facet_grid(~dose,labeller=label_both)+labs(title="Dist of Tooth Length by Supplement Type, Dose Level")+theme(legend.position="none")+xlab("Supplement Type")+ylab("Tooth Length")+theme(text = element_text(size=12))


# group and summarize by mean and standard deviation
TGstats <- ddply(ToothGrowth,.(supp,dose),summarise, mean = mean(len), sd = sd(len))
TGstats

## Hypothesize that OJ is a more effective supplement than VC at each dose level
# assume mound-shaped, symmetric distribution with unequal variances
#calculate the T confidence interval for each set

#Mean difference: dose = 0.5
OJmean <- subset(TGstats,dose==0.5 & supp=='OJ')$mean
VCmean <- subset(TGstats,dose==0.5 & supp=='VC')$mean
md <- OJmean - VCmean
sd1 <- subset(TGstats,dose==0.5 & supp=='OJ')$sd
sd2 <- subset(TGstats,dose==0.5 & supp=='VC')$sd
n1 <- n2 <- 10
df <- (sd1^2/n1 + sd2^2/n2)^2 / (((sd1^2/n1)^2/(n1-1)) + ((sd2^2/n2)/(n2-1)))
tdf <- qt(0.975,df)
tConf05 <- md + c(-1,1)*tdf*(sd1^2/n1 + sd2^2/n2)^0.5

#Mean difference: dose = 1.0
OJmean <- subset(TGstats,dose==1.0 & supp=='OJ')$mean
VCmean <- subset(TGstats,dose==1.0 & supp=='VC')$mean
md <- OJmean - VCmean
sd1 <- subset(TGstats,dose==1.0 & supp=='OJ')$sd
sd2 <- subset(TGstats,dose==1.0 & supp=='VC')$sd
n1 <- n2 <- 10
df <- (sd1^2/n1 + sd2^2/n2)^2 / (((sd1^2/n1)^2/(n1-1)) + ((sd2^2/n2)/(n2-1)))
tdf <- qt(0.975,df)
tConf1 <- md + c(-1,1)*tdf*(sd1^2/n1 + sd2^2/n2)^0.5


#Mean difference: dose = 1.0
OJmean <- subset(TGstats,dose==2.0 & supp=='OJ')$mean
VCmean <- subset(TGstats,dose==2.0 & supp=='VC')$mean
md <- OJmean - VCmean
sd1 <- subset(TGstats,dose==2.0 & supp=='OJ')$sd
sd2 <- subset(TGstats,dose==2.0 & supp=='VC')$sd
n1 <- n2 <- 10
df <- (sd1^2/n1 + sd2^2/n2)^2 / (((sd1^2/n1)^2/(n1-1)) + ((sd2^2/n2)/(n2-1)))
tdf <- qt(0.975,df)
tConf2 <- md + c(-1,1)*tdf*(sd1^2/n1 + sd2^2/n2)^0.5




