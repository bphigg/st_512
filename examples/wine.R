
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for wine example

#install.packages("emmeans")
library(emmeans)

# Read in data, print, draw basic plot

wine <- read.table(file="../Data/wine.txt", header=TRUE)
print(wine)

# Create a factor version of the variable, save to data set pea

wine$trt <- factor(wine$Treatment)

# Basic numerical & graphical summaries

table(wine$trt)
plot(Response ~ trt, data=wine, xlab="Wine")
tapply(wine$Response, wine$trt, mean)
tapply(wine$Response, wine$trt, sd)

# One-way ANOVA model fit

wine.lm <- lm(Response ~ trt, data=wine)
anova(wine.lm)
summary(wine.lm)

# Comparing the red wines 

levels(wine$trt) 

W.red <- list(
  Cab_vs_Mer = c(1, 0, 0, -1, 0, 0), 
  Mer_vs_Syr = c(0, 0, 0, 1, 0, -1) 
)

wine.emm <- emmeans(wine.lm, 'trt')
wine.contr.red <- contrast(wine.emm, W.red)

test(wine.contr.red, joint=TRUE)

# Repeat for the white wines

W.white <- list(
  Cha_vs_Ger = c(0, 1, -1, 0, 0, 0), 
  Ger_vs_Rie = c(0, 0, 1, 0, -1, 0) 
)

wine.emm <- emmeans(wine.lm, 'trt')
wine.contr.white <- contrast(wine.emm, W.white)

test(wine.contr.white, joint=TRUE)
