
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 15.5 in Ott & Longnecker

air <- read.table(file="../Data/example_15-5.txt", header=TRUE)
print(air)

air$month <- factor(air$month)
air$home <- factor(air$home)
air$brand <- factor(air$brand)

boxplot(cadr ~ brand, data=air)

o <- lm(cadr ~ month + home + brand, data=air)
anova(o)
