
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for the analysis (simple linear regression) of the beer & blood alcohol data


# Read in data, define variables, draw basic plot

beer <- read.table(file="../Data/beer.txt", header=TRUE)
print(beer)
y <- beer$BAC
x <- beer$Beers
plot(x, y)


# Fit regression model, get output (estimates and tests)

slr.out <- lm(y ~ x)
abline(slr.out, col=2)
summary(slr.out)  


# Confidence/prediction intervals for y based on new individual with given x

beer.new <- data.frame(x=6)  # data for a "new" individual who consumed x=6 beers
predict(slr.out, beer.new, interval="confidence")
predict(slr.out, beer.new, interval="prediction")


# Diagnostic plots to check model assumptions

e <- slr.out$residuals
yhat <- slr.out$fitted.values
plot(e)
plot(x, e); abline(h=0, lty=3)
plot(x, yhat); abline(h=0, lty=3)
qqnorm(e)
qqline(e)
hist(e)

