
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Exercise 15.2 in Ott & Longnecker

blue <- read.table(file="../Data/exercise_15-2.txt", header=TRUE)
print(blue)

blue$farm <- factor(blue$farm)
blue$trt <- factor(blue$trt)

boxplot(yield ~ trt, data=blue)

o <- lm(yield ~ farm + trt, data=blue)
anova(o)


