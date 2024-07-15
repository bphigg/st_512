
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 14.8 in Ott & Longnecker

# Read in data

rose <- read.table(file="../Data/example_14-8.txt", header=TRUE)
print(rose)

# Code factors as *factors*

rose$fungicide <- factor(rose$fungicide)
rose$dose <- factor(rose$dose)

# Interaction plot

with(rose, interaction.plot(fungicide, dose, weight))

# Two-way ANOVA with interaction
# Main effect SS are different -- depends on the order (Type I)

o.rose1 <- lm(weight ~ fungicide * dose, data=rose)
summary(o.rose1)
anova(o.rose1)

o.rose2 <- lm(weight ~ dose * fungicide, data=rose)
summary(o.rose2)
anova(o.rose2)

# Estimate marginal means

library(emmeans)
emmeans(o.rose1, ~ fungicide * dose)
emmeans(o.rose1, ~ fungicide)
emmeans(o.rose1, ~ dose)

tapply(rose$weight, rose$fungicide, mean)
tapply(rose$weight, rose$dose, mean)

