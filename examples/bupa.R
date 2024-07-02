
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for the analysis (simple linear regression) of the BUPA data
## Data comes from: http://archive.ics.uci.edu/ml/datasets/Liver+Disorders


# Read in data, define variables, draw basic plot

bupa <- read.table(file="../Data/bupa.txt", header=FALSE, sep=",")
print(bupa[1:5,])
y <- bupa$V3
x <- log(bupa$V5)
plot(x, y)


# Fit regression model, draw some plots to check assumptions

lm1 <- lm(y ~ x)
summary(lm1)
abline(lm1, col=2)
plot(x, lm1$residuals); abline(h=0, lty=3)
qqnorm(lm1$residuals); qqline(lm1$residuals)


# Make transformation y --> log(y) and try again

yy <- log(y)
lm2 <- lm(yy ~ x)
plot(x, yy)
abline(lm2, col=2)
plot(x, lm2$residuals); abline(h=0, lty=3)
qqnorm(lm2$residuals); qqline(lm2$residuals)


