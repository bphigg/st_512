
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Exercise 14.23 in Ott & Longnecker

# Read in data

trunk <- read.table(file="../Data/exercise_14-23.txt", header=TRUE)
print(trunk)

# Marginal summaries

tapply(trunk$diam, trunk$ph, mean)
tapply(trunk$diam, trunk$cal, mean)

# Code factors as *factors*

trunk$ph <- factor(trunk$ph)
trunk$cal <- factor(trunk$cal)

# Graphical summaries

boxplot(diam ~ ph * cal, data=trunk)
with(trunk, interaction.plot(ph, cal, diam))

# Two-way ANOVA model with interaction

o <- lm(diam ~ ph * cal, data=trunk)
summary(o)
anova(o)

# Test for interaction with a reduced model & anova()

oo <- lm(diam ~ ph + cal, data=trunk)
summary(oo)
anova(oo)
anova(oo, o)
