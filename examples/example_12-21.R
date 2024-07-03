
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for (variation on) Example 12.21 in Ott & Longnecker

# Read in data

rats <- read.table(file="../Data/example_12-21_mod.txt", header=TRUE)
print(rats)

# Manual coding of dummy variable, partial F test

dummy.B <- as.numeric(rats$drug == 'B')
dummy.C <- as.numeric(rats$drug == 'C')

o.full <- lm(rats$score ~ rats$dose + dummy.B + dummy.C)
o.red <- lm(rats$score ~ rats$dose)
summary(o.full)
anova(o.full)
summary(o.red)
anova(o.red)

anova(o.red, o.full)

# Tell R that the variable is categorical, let it do the work

o.full <- lm(score ~ dose + factor(drug), data=rats)
o.red <- lm(score ~ dose, data=rats)
#summary(o)
anova(o.red, o.full)

# R will recognize categorical/factors if expressed as characters

summary(lm(score ~ dose + drug, data=rats))

# But it won't if coding of levels is numerical...
# The following analysis is WRONG!

drug.num <- 1 * (rats$drug == 'A') + 2 * (rats$drug == 'B') + 3 * (rats$drug == 'C')
o.wrong <- lm(rats$score ~ rats$dose + drug.num)
summary(o.wrong)


