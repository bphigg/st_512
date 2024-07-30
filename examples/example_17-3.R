 cv
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 17.3 in Ott & Longnecker

food <- read.table(file="data/example_17-3.txt", header=TRUE)
print(food)

food$assay <- factor(food$assay)
food$lab <- factor(food$lab)

tapply(food$calcium, food$lab, mean)
tapply(food$calcium, food$assay, mean)

boxplot(calcium ~ lab, data=food)
boxplot(calcium ~ assay, data=food)

boxplot(calcium ~ lab * assay, data=food)
with(food, interaction.plot(lab, assay, calcium))

library(EMSaov)
o <- EMSanova(calcium ~ lab * assay, data=food, type=c("R", "R"))
print(o)

library(lme4)
oo <- lmer(calcium ~ (1 | lab) + (1 | assay) + (1 | lab:assay), data=food)
summary(oo)

fitted.lmer <- cbind(assay=food$assay, 
                     lab=food$lab, 
                     fit.lmer=fitted.values(oo))[1:18 %% 2 == 1,]
boxplot(calcium ~ lab * assay, data=food)
points(fitted.lmer[,3], col=2, pch="X")

# Reduced model 
ooo <- lmer(calcium ~ (1 | lab) + (1 | assay), data=food)
summary(ooo)
anova(ooo, oo)

