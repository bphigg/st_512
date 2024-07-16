### HW 3

## 8.32

corn <- read.table(file="data/512hw03_prob1.txt", header=TRUE)
print(corn)
## a)
tapply(corn$yield, corn$corn, mean)
tapply(corn$yield, corn$corn, sd)

## b) find a 95% confidence interval for the mean yield for each corn variety
corn_means <- tapply(corn$yield, corn$corn, mean)
corn_stds <- tapply(corn$yield, corn$corn, sd)
t_star <- qt(0.975, 7)
typeA <- c(corn_means[1], corn_means[1] + c(1,-1)*t_star*(corn_stds[1]/sqrt(7)))
typeB <- c(corn_means[2], corn_means[2] + c(1,-1)*t_star*(corn_stds[2]/sqrt(7)))
typeC <- c(corn_means[3], corn_means[3] + c(1,-1)*t_star*(corn_stds[3]/sqrt(7)))
typeD <- c(corn_means[4], corn_means[4] + c(1,-1)*t_star*(corn_stds[4]/sqrt(7)))
cbind(typeA, typeB, typeC, typeD)
rbind(typeA, typeB, typeC, typeD)
data.frame(index=c('mean', 'upper', 'lower'), typeA, typeB, typeC, typeD)
data.frame(typeA, typeB, typeC, typeD, row.names=c('mean', 'upper', 'lower'))

# Looking at the means and CI's for the four types of corn, I would infer that the four corn varieties are NOT the same. The means of TypeB and Type C exceed the upper CI of the means for TypeA and TypeD. This would indicate that the means of all four corn varieties are not equal - there is a difference in yield between types.

boxplot(yield ~ corn, data=corn)

## c)
o <- lm(yield ~ corn, data = corn)
summary(o)
anova(o)

# The overall F-test for the model confirms our suspicions in part b) - the low p-value of 5.85e-05 indicates an almost zero plausibility that the means of all four corn varieties are the same. So we can reject the null hypothesis and accept the alternative hypothesis that there is a difference between the means.

## 8.39

library(emmeans)

peach <- read.table(file="data/512hw03_prob2.txt", header=TRUE)
print(peach)

peach$herb <- factor(peach$herb)
levels(peach$herb)

## a) run an ANOVA for detecting differences in seedling heights for all three groups

peach_lm <- lm(height ~ herb, data=peach)
summary(peach_lm)
anova(peach_lm)

## b) test if the mean seedling height for each herbicide is different from the control

peach_emm <- emmeans(peach_lm, "herb")
peach_contr <- contrast(peach_emm, list("ctrl_vs_nem"=c(1, -1, 0), "ctrl_vs_no_nem"=c(1,0,-1), 
                                        "nem_vs_no_nem"=c(0,1,-1)))
summary(peach_contr, infer=c(TRUE, TRUE)) 

