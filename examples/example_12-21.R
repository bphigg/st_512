
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 12.21 in Ott & Longnecker

# Read in data

rats <- read.table(file="data/example_12-21.txt", header=TRUE)
print(rats)

y <- rats$score
x1 <- rats$drug == 'A'
x2 <- rats$dose

plot(y ~ x2, col=1 + x1)
plot(y ~ jitter(x2), col=1 + x1) # better visualization 

x3 <- x1 * x2
o <- lm(y ~ x1 + x2 + x3)
summary(o)
abline(a=o$coefficients[1], b=o$coefficients[3])
abline(a=sum(o$coefficients[1:2]), b=sum(o$coefficients[3:4]), col=2)

summary(lm(y~x2*x1))

# Separate simple linear regression models to the groups
## same fitted lines
## joint model has only one error variance to estimate...

o_ <- lm(y ~ x2, subset=(x1==0))
o__ <- lm(y ~ x2, subset=(x1==1))
summary(o_)
summary(o__)

abline(lm(y ~ x2, subset=(x1==0)), lty=2, col=1)
abline(lm(y ~ x2, subset=(x1==1)), lty=2, col=2)



