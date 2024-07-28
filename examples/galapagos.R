
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Galapagos example in Faraway's book

library(faraway) 

data(gala)
head(gala)
?gala

# Naive linear model

o.lm <- lm(Species ~ ., data=gala)
summary(o.lm)
plot(residuals(o.lm) ~ fitted(o.lm)); abline(h=0, lty=3)

# Poisson log-linear model

o.glm <- glm(Species ~ ., family=poisson, data=gala)
summary(o.glm)
oo.glm <- glm(Species ~ . - Adjacent - Scruz, family=poisson, data=gala)
anova(oo.glm, o.glm)
print(pval <- 1 - pchisq(2.5194, df=2))







