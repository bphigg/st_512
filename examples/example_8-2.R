
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 8.2 in Ott & Longnecker

# Read in data

hostility <- read.table(file="data/example_8-2.txt", header=TRUE)
print(hostility)

# Numerical and graphical summaries

tapply(hostility$score, hostility$method, summary)
tapply(hostility$score, hostility$method, mean)
tapply(hostility$score, hostility$method, sd)

boxplot(score ~ method, data=hostility)
abline(h=mean(hostility$score), col=2, lty=2)

# Code "method" as a factor

trt <- factor(hostility$method)
y <- hostility$score

print(trt)

o <- lm(y ~ trt)
summary(o)

# Compare to linear regression with dummies

d2 <- as.numeric(hostility$method == 2)
d3 <- as.numeric(hostility$method == 3)

summary(lm(y ~ d2 + d3))

# Check residual diagnostic plots

plot(o$fitted.values, o$residuals); abline(h=0, lty=3)
qqnorm(o$residuals); qqline(o$residuals)
