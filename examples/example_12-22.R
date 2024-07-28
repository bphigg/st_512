
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 12.22 in Ott & Longnecker

ck <- read.table(file="../Data/example_12-22.txt", header=TRUE)
print(ck)

plot(yes / (yes + no) ~ ck, data=ck)

o.glm <- glm(cbind(yes, no) ~ ck, family=binomial, data=ck)
summary(o.glm)

x <- seq(20, 500, length=100)
g <- function(z) exp(z) / (1 + exp(z))
lines(x, g(o.glm$coefficients[1] + o.glm$coefficients[2] * x), col=2)

predict(o.glm, data.frame(ck=200), type="response")

