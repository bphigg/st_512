
## ST512 -- Lab 08
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Problem 1

prob1 <- read.table(file="512lab08_prob1.txt", header=TRUE)

tapply(prob1$life, prob1$material, mean)
tapply(prob1$life, prob1$temp, mean)

prob1$material <- factor(prob1$material)
prob1$temp <- factor(prob1$temp)

with(prob1, interaction.plot(temp, material, life))

o <- lm(life ~ material + temp, data=prob1)
summary(o)
anova(o)

plot(o$fitted.values, o$residuals); abline(h=0, lty=3)

oo <- lm(life ~ material * temp, data=prob1)
anova(oo)

plot(oo$fitted.values, oo$residuals); abline(h=0, lty=3)

# Problem 2

prob2 <- read.table(file="512lab08_prob2.txt", header=TRUE)
print(prob2)

prob2$group <- factor(prob2$group)

boxplot(score ~ group, data=prob2)

o <- lm(score ~ group, data=prob2)
summary(o)
anova(o)

## In "correct" order for Type I to match what we want

oo.1 <- lm(score ~ age + group, data=prob2)
anova(oo.1)

## In "incorrect" order, but with manual partial F test

oo.2 <- lm(score ~ group + age, data=prob2)
ooo <- lm(score ~ age, data=prob2)
anova(ooo, oo.2)

library(emmeans)
emmeans(oo.1, ~ group)




