
## ST512 -- Week 13 lab
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

library(lme4)
library(EMSaov)

# Problem 1

prob1 <- read.table(file="lab13_prob1.txt", header=TRUE)
print(prob1)

prob1$fert <- factor(prob1$fert)
prob1$seed <- factor(prob1$seed)

o.lmer <- lmer(yield ~ fert + (1 | seed) + (1 | fert:seed), data=prob1)
summary(o.lmer)

with(prob1, interaction.plot(fert, seed, yield))

oo.lmer <- lmer(yield ~ fert + (1 | seed), data=prob1)
summary(oo.lmer)
anova(oo.lmer, o.lmer)


# Problem 2

prob2 <- read.table(file="lab13_prob2.txt", header=TRUE)
print(prob2)

prob2$supplier <- factor(prob2$supplier)
prob2$batch <- factor(prob2$batch)

EMSanova(purity ~ batch + supplier, data=prob2, type=c("R", "F"), nested=c("supplier", NA))

o.lmer <- lmer(purity ~ supplier + (1 | supplier:batch), data=prob2)
summary(o.lmer)

FF <- (2.639 + 3 * 1.710) / 2.639
pval <- 1 - pf(FF, df1=9, df2=24)
cbind(F=FF, p.value=pval)


