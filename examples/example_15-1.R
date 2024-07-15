
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 15.1 in Ott & Longnecker

# Read in data

bean <- read.table(file="../Data/example_15-1.txt", header=TRUE)
print(bean)

# Code factors as *factors*

bean$trt <- factor(bean$trt)
bean$block <- factor(bean$block)

# Two-way ANOVA model without interaction

o <- lm(y ~ block + trt, data=bean)
summary(o)

library(emmeans)
emmeans(o, ~ trt)
tapply(bean$y, bean$trt, mean)

# Partial F test for treatment effect

anova(o)


