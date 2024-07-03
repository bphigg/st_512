
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for the birth weight data analysis in Week 04a

# Read in data

library(MASS)
data(birthwt)
?birthwt 
head(birthwt)

# Fit full model 

o.full <- lm(bwt ~ ., data=birthwt)
summary(o.full)
AIC(o.full)

# Fit a reduced model

o.red <- lm(bwt ~ . - ftv - age, data=birthwt)
summary(o.red)
AIC(o.red)

# Find all "good" models

install.packages("leaps")
library(leaps)
full <- regsubsets(bwt ~ ., data=birthwt, nbest=1, nvmax=9)
info <- summary(full)
regsubsets.out <- cbind(R2=info$rsq, adjR2=info$adjr2, bic=info$bic, rss=info$rss)
aic <- info$bic - (1:9) * log(nrow(birthwt)) + 2 * (1:9)
cbind(info$which[,-1], round(regsubsets.out, 3), aic=round(aic, 3))
plot(1:9, info$bic, type="l", xlab="number of variables", ylab="BIC")
plot(1:9, aic, type="l", xlab="number of variables", ylab="AIC")



