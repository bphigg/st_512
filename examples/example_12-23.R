
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 12.23 in Ott & Longnecker

plasma <- read.table(file="../Data/example_12-23.txt", header=TRUE)
print(plasma)

o.glm <- glm(cbind(hth, 1 - hth) ~ fib + gam, family=binomial, data=plasma)
summary(o.glm)
oo.glm <- glm(cbind(hth, 1 - hth) ~ fib, family=binomial, data=plasma)
anova(oo.glm, o.glm)
print(pval <- 1 - pchisq(1.8692, df=1))

# We can't expect to see "good fit" when y = 0 or 1 only 

plot(hth ~ fib, data=plasma)
x <- seq(min(plasma$fib), max(plasma$fib), length=100)
g <- function(z) exp(z) / (1 + exp(z))
lines(x, g(oo.glm$coefficients[1] + oo.glm$coefficients[2] * x), col=2)
