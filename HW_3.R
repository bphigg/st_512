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
typeA <- corn_means[1] + c(1,-1)*t_star*(corn_stds[1]/sqrt(7))
typeB <- corn_means[2] + c(1,-1)*t_star*(corn_stds[2]/sqrt(7))
typeC <- corn_means[3] + c(1,-1)*t_star*(corn_stds[3]/sqrt(7))
typeD <- corn_means[4] + c(1,-1)*t_star*(corn_stds[4]/sqrt(7))
cbind(typeA, typeB, typeC, typeD)

boxplot(yield ~ corn, data=corn)

## c)
o <- lm(yield ~ corn, data = corn)
summary(o)