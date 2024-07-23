
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for irrigation example in Faraway's book

library(lme4)
library(lmerTest)
library(faraway)
#install.packages("faraway")

data(irrigation)
?irrigation
print(irrigation)

with(irrigation, interaction.plot(variety, irrigation, yield))

o <- lmer(yield ~ variety * irrigation + (1 | field), data=irrigation)
summary(o, correlation=FALSE)
anova(o)

oo <- lm(yield ~ variety + irrigation, data=irrigation)
summary(oo, correlation=FALSE)
anova(oo)
