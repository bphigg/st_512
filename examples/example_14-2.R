
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 14.2 in Ott & Longnecker

# Read in data

paint <- read.table(file="../Data/example_14-2.txt", header=TRUE)
print(paint)

# Numerical and graphical summaries

tapply(paint$y, paint$paint, mean)
tapply(paint$y, paint$paint, sd)

boxplot(y ~ paint, data=paint)
abline(h=mean(paint$y), col=2, lty=2)

# Code "method" as a factor

pnt <- factor(paint$paint)
y <- paint$y

print(pnt)

o <- lm(y ~ pnt)
o.sum <- summary(o)
print(o.sum)

anova(o)

# Check residual diagnostic plots

plot(o$fitted.values, o$residuals); abline(h=0, lty=3)
qqnorm(o$residuals); qqline(o$residuals)

# Contrast: compare avg of new paints (B, C, D) with old paint (A)

w <- c(1, -1/3, -1/3, -1/3)
theta.hat <- sum(w * as.numeric(tapply(y, pnt, mean)))
print(theta.hat)

r <- as.numeric(tapply(y, pnt, length))
SS.w <- theta.hat**2 / sum(w**2 / r)
F.stat <- SS.w / o.sum$sigma**2
df1 <- 1
df2 <- length(y) - length(levels(pnt))
pval <- 1 - pf(F.stat, df1, df2); print(pval)
