
## ST512 -- Week 14 lab
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

library(lme4)
library(lmerTest)
library(MASS)

data(oats)
?oats
print(oats)

str(oats)

with(oats, interaction.plot(N, V, Y))

o <- lmer(Y ~ N * V + (1 | B) + (1 | V:B), data=oats)
summary(o, correlation=FALSE)
anova(o)

oo <- lmer(Y ~ N + V + (1 | B) + (1 | V:B), data=oats)
summary(oo, correlation=FALSE)
anova(oo)
