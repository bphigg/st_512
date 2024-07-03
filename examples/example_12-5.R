
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 12.5 in Ott & Longnecker

# Read in data

compound <- read.table(file="../Data/example_12-5.txt", header=TRUE)
print(compound[1:5,])

# Pairwise investigations

plot(compound)
cor(compound)

# Multiple linear regression model

o <- lm(loss ~ time + humid, data=compound)
summary(o)
anova(o)

# Plots to evaluate model assumptions 

plot(o$residuals ~ o$fitted.values); abline(h=0)
qqnorm(o$residuals); qqline(o$residuals)
