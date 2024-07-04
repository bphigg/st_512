
## ST512 -- Lab 04
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Part 0 & 1

mydata <- read.table(file="512lab04.txt", header=TRUE)
y <- mydata$y
x1 <- mydata$x1
x2 <- mydata$x2

# Part 2

x1.x2 <- x1 * x2

o.1 <- lm(y ~ x1 + x2 + x1.x2)
summary(o.1)

o.2 <- lm(y ~ x1 + x2)
summary(o.2)
anova(o.2, o.1)

# Part 3

plot(o.2$fitted.values, o.2$residuals); abline(h=0, lty=3)

x1.2 <- x1^2
x2.2 <- x2^2
o.3 <- lm(y ~ x1 + x2 + x1.2 + x2.2)
summary(o.3)
plot(o.3$fitted.values, o.3$residuals); abline(h=0, lty=3)
anova(o.3, o.2)

