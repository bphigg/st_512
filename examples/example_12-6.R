
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 12.6 in Ott & Longnecker

# Read in data

fitness <- read.table(file="../Data/example_12-6.txt", header=TRUE)
print(fitness)

# Pairwise investigations

plot(fitness)
cor(fitness)

# Multiple linear regression model

o <- lm(oxygen ~ weight + age + time + rate, data=fitness)
summary(o)
anova(o)

# Plots to evaluate model assumptions

plot(o$residuals ~ o$fitted.values); abline(h=0)
qqnorm(o$residuals); qqline(o$residuals)
