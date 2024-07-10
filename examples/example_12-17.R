
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 12.17 in Ott & Longnecker

# Read in data, print, draw basic plot

bass <- read.table(file="data/example_12-17.txt", header=TRUE)
print(bass)
plot(bass)

o.full <- lm(catch ~ residence + size + access + structure, data=bass)
o.red <- lm(catch ~ residence + size, data=bass)
anova(o.full)
anova(o.red)

anova(o.red, o.full)
