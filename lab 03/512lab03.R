
## ST512 -- Lab 03
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Part 1

cheese <- read.table(file="cheese.txt", header=TRUE)
cor(cheese)
plot(cheese)
out.1 <- lm(taste ~ h2s, data=cheese)
summary(out.1)
plot(out.1$fitted.values, out.1$residuals, xlab="fitted", ylab="residuals")
abline(h=0, lty=3)
qqnorm(out.1$residuals, main=""); qqline(out.1$residuals)

# Part 2

out.2 <- lm(taste ~ acetic + h2s + lactic, data=cheese)
summary(out.2)
confint(out.2)
op <- par(mfrow=c(2, 2))
plot(y=out.2$residuals, x=cheese$acetic); abline(h=0, lty=3)
plot(y=out.2$residuals, x=cheese$h2s); abline(h=0, lty=3)
plot(y=out.2$residuals, x=cheese$lactic); abline(h=0, lty=3)
qqnorm(out.2$residuals, main=""); qqline(out.2$residuals)
par(op)

# Part 3

new <- data.frame(acetic=10, h2s=5.5, lactic=4)
predict(out.2, new, interval="pred", level=0.90)
