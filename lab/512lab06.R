
## ST512 -- Lab 06
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

cotton <- read.table(file="512lab06.txt", header=TRUE)

# Part 1

boxplot(strength ~ weight, data=cotton)

# Part 4

wt <- factor(cotton$weight)
strength <- cotton$strength
o <- lm(strength ~ wt)

plot(o$fitted.values, o$residuals); abline(h=0, lty=3)
qqnorm(o$residuals); qqline(o$residuals)

# Part 5

o.sum <- summary(o)
print(o.sum)
anova(o)

# Part 6(b)

w.b <- c(0, 1, 0, -1, 0)
theta.hat <- sum(w.b * as.numeric(tapply(strength, wt, mean)))
r <- as.numeric(tapply(strength, wt, length))
SS.wb <- theta.hat**2 / sum(w.b**2 / r)
F.statb <- SS.wb / o.sum$sigma**2
df1 <- 1
df2 <- length(strength) - length(levels(wt))
pval.b <- 1 - pf(F.statb, df1, df2)
print(pval.b)

# Part 6(c)

w.c <- c(-0.5, 1, 0, 0, -0.5)
theta.hat <- sum(w.c * as.numeric(tapply(strength, wt, mean)))
r <- as.numeric(tapply(strength, wt, length))
SS.wc <- theta.hat**2 / sum(w.c**2 / r)
F.statc <- SS.wc / o.sum$sigma**2
df1 <- 1
df2 <- length(strength) - length(levels(wt))
pval.c <- 1 - pf(F.statc, df1, df2)
print(pval.c)

