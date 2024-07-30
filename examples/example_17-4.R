
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 17.4 in Ott & Longnecker

sun <- read.table(file="data/example_17-4.txt", header=TRUE)
print(sun)

sun$sunscreen <- factor(sun$sunscreen)
sun$tech <- factor(sun$tech)

with(sun, interaction.plot(sunscreen, tech, y))

library(EMSaov)
library(lme4)

o.ems <- EMSanova(y ~ sunscreen * tech, data=sun, type=c("F", "R"))
print(o.ems)

o.lmer <- lmer(y ~ sunscreen + (1 | tech) + (1 | sunscreen:tech), data=sun)
summary(o.lmer)

oo.lmer <- lmer(y ~ sunscreen + (1 | tech), data=sun)
summary(oo.lmer)

anova(oo.lmer, o.lmer)

# Same test for interaction as above but now with lmerTest

library(lmerTest)

o.lmer <- lmer(y ~ sunscreen + (1 | tech) + (1 | sunscreen:tech), data=sun)
summary(o.lmer)
ranova(o.lmer)

