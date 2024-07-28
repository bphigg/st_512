
## ST512 -- Week 15 lab
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Problem 1

prob1 <- read.table(file="lab15_prob1.txt", header=TRUE)
print(prob1)

o.lm <- lm(y ~ exp, data=prob1)
summary(o.lm)
plot(y ~ exp, data=prob1)
abline(o.lm, col=2)

o.glm <- glm(cbind(y, 1 - y) ~ exp, data=prob1, family=binomial)
summary(o.glm)

x <- seq(2, 32, length=50)
g <- function(z) exp(z) / (1 + exp(z))
#plot(y ~ exp, data=prob1)
lines(x, g(o.glm$coefficients[1] + o.glm$coefficients[2] * x), col=4)

x0 <- c(1, 25)
linear.part <- sum(x0 * o.glm$coefficients)
p.hat <- g(linear.part)
Sigma <- summary(o.glm)$cov.unscaled
se <- as.numeric( sqrt( t(x0) %*% Sigma %*% x0 ) )
cbind(p.hat=p.hat, lower=g(linear.part - 1.96 * se), upper=g(linear.part + 1.96 * se))


# Problem 2

prob2 <- read.table(file="lab15_prob2.txt", header=TRUE)
print(prob2)

prob2$type <- factor(prob2$type)
prob2$site <- factor(prob2$site)

xt <- xtabs(y ~ type + site, data=prob2)
print(xt)

o <- glm(y ~ type * site, family=poisson, data=prob2)
summary(o)
data.frame(type=prob2$type, site=prob2$site, y=prob2$y, fitted=predict(o, type="response"))

oo <- glm(y ~ type + site, family=poisson, data=prob2)
summary(oo)
anova(oo, o)
print(1 - pchisq(51.795, df=6))

summary(xt)

data.frame(type=prob2$type, site=prob2$site, y=prob2$y, fitted=predict(oo, type="response"))
