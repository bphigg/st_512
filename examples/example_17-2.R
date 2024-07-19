
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 17.2 in Ott & Longnecker

plaque <- read.table(file="../Data/example_17-2.txt", header=TRUE)
print(plaque)

plaque$subject <- factor(plaque$subject)
plaque$analyst <- factor(plaque$analyst)

boxplot(dna ~ analyst, data=plaque)

# Incorrect fixed effect analysis 
o <- lm(dna ~ subject + analyst, data=plaque)
summary(o)
anova(o)

# Correct random effect analysis 
library(lme4)

oo <- lmer(dna ~ (1 | subject) + (1 | analyst), data=plaque)
summary(oo)

hat.sigma2.eps <- 0.02474
hat.sigma2.tau <- 0.11176
FF <- (hat.sigma2.eps + 10 * hat.sigma2.tau) / hat.sigma2.eps
cbind(F=FF, p.value=1 - pf(FF, df1=4, df2=36))

library(EMSaov)
ooo <- EMSanova(dna ~ subject + analyst, data=plaque, type=c("R", "R"))
print(ooo)

