
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 18.1 in Ott & Longnecker

library(lme4)
library(lmerTest)

soybean <- read.table(file="data/example_18-1.txt", header=TRUE)
print(soybean)

soybean$field <- factor(soybean$field)
soybean$phos <- factor(soybean$phos)
soybean$variety <- factor(soybean$variety)

with(soybean, interaction.plot(phos, variety, yield))

o <- lmer(yield ~ variety * phos + (1 | field) + (1 | variety:field), data=soybean)
summary(o, correlation=FALSE)
anova(o)
