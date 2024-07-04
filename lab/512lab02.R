
## ST512 -- Lab 01
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Part 1: SLR and grape data

grapes <- read.table(file="grapes.txt", header=TRUE)
nclust <- grapes$nclust
yield <- grapes$yield
out <- lm(yield ~ nclust)
summary(out)
predict(out, data.frame(nclust=102), interval="conf")
plot(nclust, out$residuals); abline(h=0, lty=3)


# Part 2: beyond the linear model

height <- c(68, 61, 63, 70, 69, 65, 72)
weight <- c(155, 99, 115, 205, 170, 125, 220)
plot(x=height, y=weight)
out <- lm(weight ~ height)
plot(height, out$residuals); abline(h=0, lty=3)
height2 <- height^2
out2 <- lm(weight ~ height + height2)
plot(x=height, y=weight)
beta.hat <- out2$coefficients
fitted.line <- function(x) beta.hat[1] + beta.hat[2] * x + beta.hat[3] * x^2
curve(fitted.line, range(height), add=TRUE, col=2)
plot(height, out2$residuals); abline(h=0, lty=3)


# Part 3: transformations

dose <- c(2, 2, 4, 4, 8, 8, 16, 16, 32, 32)
rate <- c(60, 58, 63, 62, 67, 65, 70, 70, 74, 73)
plot(dose, rate)
log.dose <- log(dose)
plot(log.dose, rate)
out <- lm(rate ~ log.dose)
summary(out)
plot(y=out$residuals, x=out$fitted.values); abline(h=0, lty=3)

