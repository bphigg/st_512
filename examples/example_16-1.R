
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 16.1 in Ott & Longnecker

# Read in data

peanut <- read.table(file="data/example_16-1.txt", header=TRUE)
print(peanut)

# Code factors as *factors*

peanut$type <- factor(peanut$type)

# Fit the ANCOVA model
# anova() to test for interaction & treatment effect

plot(yield ~ height, col=as.numeric(type), data=peanut)

o <- lm(yield ~ height * type, data=peanut)
summary(o)
anova(o)

oo <- lm(yield ~ height + type, data=peanut)
summary(oo)
anova(oo)

# Estimate marginal means

library(emmeans)
emmeans(oo, ~ type)
tapply(peanut$yield, peanut$type, mean)

ooo <- lm(yield ~ type, data=peanut)
summary(ooo)

anova(ooo, oo)
