
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for the SECOND birth weight data analysis in Week 04b

# Read in data

library(MASS)
data(birthwt)
?birthwt 
head(birthwt)

#install.packages("leaps")
library(leaps)
full <- regsubsets(bwt ~ . - race + factor(race), data=birthwt, nbest=1, nvmax=10)
info <- summary(full)
regsubsets.out <- cbind(R2=info$rsq, adjR2=info$adjr2, bic=info$bic, rss=info$rss)
aic <- info$bic - (1:10) * log(nrow(birthwt)) + 2 * (1:10)
cbind(info$which[,-1], round(regsubsets.out, 3), aic=round(aic, 3))
plot(1:10, info$bic, type="l", xlab="number of variables", ylab="BIC")
plot(1:10, aic, type="l", xlab="number of variables", ylab="AIC")

# Take the "best" model based on AIC

mymodel <- lm(bwt ~ low + smoke + factor(race) + ui, data=birthwt)
summary(mymodel)
plot(mymodel$fitted.values, mymodel$residuals); abline(h=0, lty=3)
plot(birthwt$low, mymodel$residuals); abline(h=0, lty=3)
plot(birthwt$race, mymodel$residuals); abline(h=0, lty=3)
plot(birthwt$smoke, mymodel$residuals); abline(h=0, lty=3)
plot(birthwt$ui, mymodel$residuals); abline(h=0, lty=3)
qqnorm(mymodel$residuals); qqline(mymodel$residuals)

