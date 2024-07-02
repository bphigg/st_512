
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 11.10 in Ott & Longnecker


# Read in data, define variables, draw basic plot
# x = temp
# y = heatloss

x <- c(20, 20, 20, 30, 30, 30, 40, 40, 40, 50, 50, 50, 60, 60, 60)
y <- c(86, 80, 77, 78, 84, 75, 78, 69, 76, 62, 53, 57, 33, 38, 43)
plot(x, y)


# Fit simple linear regression model

slr <- lm(y ~ x)
abline(slr, col=2)
summary(slr)  


# Diagnostic plots to check model assumptions

e <- slr$residuals
yhat <- slr$fitted.values

plot(yhat, e); abline(h=0, lty=3)   # check for linearity
plot(x, e); abline(h=0, lty=3)      # check for constant variance
#hist(e)                             # check for normality
qqnorm(e); qqline(e)                # ditto, but better than above


# Curvature is present, try a simple quadratic regression model

x2 <- x^2
sqr <- lm(y ~ x + x2)
summary(sqr)
lse <- sqr$coefficients
f <- function(x) lse[1] + lse[2] * x + lse[3] * x**2
plot(x, y)
abline(slr, col=2)
curve(f, add=TRUE, col=4)

plot(sqr$fitted.values, sqr$residuals); abline(h=0, lty=2)

