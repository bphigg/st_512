
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for o-ring example in Faraway's book

library(faraway) 

data(orings)
head(orings)
?orings

plot(damage / 6 ~ temp, xlim=c(25, 85), ylim=c(0,1), xlab="Temp", 
     ylab="O-ring failure prob", data=orings)

# Naive linear regression

o.lm <- lm(damage / 6 ~ temp, data=orings)
abline(o.lm, lty=2)

# Logistic regression 

o.glm <- glm(cbind(damage, 6 - damage) ~ temp, family=binomial, data=orings)
o.glm.sum <- summary(o.glm)
o.glm.sum
x <- 25:85
g <- function(z) exp(z) / (1 + exp(z))
lines(x, g(o.glm$coefficients[1] + o.glm$coefficients[2] * x), col=2)

predict(o.glm, data.frame(temp=31), type="response")









