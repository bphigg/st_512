
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 11-4 in Ott & Longnecker


# Read in data, define variables, draw basic plot
# x = ph level
# y = growth retardation

x <- c(3.3, 3.4, 3.4, 3.5, 3.6, 3.6, 3.7, 3.7, 3.8, 3.8, 3.9, 4.0, 4.1, 4.2, 4.3, 
       4.4, 4.5, 5.0, 5.1, 5.2)
y <- c(17.78, 21.59, 23.84, 15.13, 23.45, 20.87, 17.78, 20.09, 17.78, 12.46, 14.95, 
       15.87, 17.45, 14.35, 14.64, 17.25, 12.57, 7.15, 7.50, 4.34)
plot(x, y)


# Fit regression model, get output (estimates and tests)

slr.out <- lm(y ~ x)
abline(slr.out, col=2)
summary(slr.out)  

confint(slr.out, "x")

n <- length(y)
t.star <- qt(0.975, df=n-2)
-7.859 + c(-1, 1) * t.star * 1.090 


# Confidence/prediction intervals for ret based on new soil with given ph

new <- data.frame(x=c(4.0, 4.7))  
predict(slr.out, new, interval="confidence")
predict(slr.out, new, interval="prediction")


# Diagnostic plots to check model assumptions

e <- slr.out$residuals
yhat <- slr.out$fitted.values

plot(yhat, e); abline(h=0, lty=3)   # check for linearity
plot(e)                             # check for independence
plot(x, e); abline(h=0, lty=3)      # check for constant variance
hist(e)                             # check for normality
qqnorm(e); qqline(e)                # ditto, but better than above


