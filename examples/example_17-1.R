
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 17.1 in Ott & Longnecker

lightning <- read.table(file="data/example_17-1.txt", header=TRUE)
print(lightning)

lightning$station <- factor(lightning$station)
boxplot(intensity ~ station, data=lightning)

# Incorrect (fixed-effect) analysis 
o <- lm(intensity ~ station, data=lightning)
summary(o)
anova(o)

# Correct (random-effect) analysis 
library(lme4)

oo <- lmer(intensity ~ (1 | station), data=lightning)
summary(oo)

hat.sigma2.eps <- 7332468
hat.sigma2.tau <- 559462
FF <- (hat.sigma2.eps + 5 * hat.sigma2.tau) / hat.sigma2.eps
cbind(F=FF, p.value=1 - pf(FF, df1=2, df2=12))

o.fit <- tapply(lightning$intensity, lightning$station, mean)
oo.fit <- unique(fitted.values(oo))
cbind(station=1:3, fitted.lm=o.fit, fitted.lmer=oo.fit)

## Follow-up in Week 12b

library(EMSaov)
ooo <- EMSanova(intensity ~ station, data=lightning, type=c("R"))
print(ooo)

