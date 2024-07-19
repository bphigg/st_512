
## ST512 -- Lab 10 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

library(lme4)
library(EMSaov)


# Problem 1

prob1 <- read.table(file="lab12_prob1.txt", header=TRUE)
print(prob1)

prob1$loom <- factor(prob1$loom)
boxplot(strength ~ loom, data=prob1)

o <- lmer(strength ~ (1 | loom), data=prob1)
summary(o)

# copy-and-paste variance estimates from lmer output
hat.sigma2.tau <- 6.958
hat.sigma2.eps <- 1.896
prop.tau.variation <- hat.sigma2.tau / (hat.sigma2.tau + hat.sigma2.eps)
print(prop.tau.variation)

FF <- (hat.sigma2.eps + 4 * hat.sigma2.tau) / hat.sigma2.eps
cbind(F=FF, p.value=1 - pf(FF, df1=3, df2=9))


# Problem 2

prob2 <- read.table(file="lab12_prob2.txt", header=TRUE)
print(prob2)

prob2$part <- factor(prob2$part)
prob2$operator <- factor(prob2$operator)

with(prob2, interaction.plot(part, operator, y))

o.ems <- EMSanova(y ~ part * operator, data=prob2, type=c("R", "R)"))
print(o.ems)

# note the warning -- and the weird variance component estimate!
o.lmer <- lmer(y ~ (1 | part) + (1 | operator) + (1 | part:operator), data=prob2)
summary(o.lmer) 

# reduced model without interaction
oo.lmer <- lmer(y ~ (1 | part) + (1 | operator), data=prob2)
summary(oo.lmer)

# var component for "operator" is real small, try removing it
ooo.lmer <- lmer(y ~ (1 | part), data=prob2)
anova(ooo.lmer, oo.lmer)


