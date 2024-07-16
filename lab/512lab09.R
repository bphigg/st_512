
## ST512 -- Lab 09
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Problem 1

prob1 <- read.table(file="512lab09_prob1.txt", header=TRUE)

prob1$type <- factor(prob1$type)
prob1$plant <- factor(prob1$plant)

boxplot(y ~ type, data=prob1)

o <- lm(y ~ type + plant, data=prob1)
anova(o)

library(emmeans)

o.emm <- emmeans(o, "type")
o.pairs <- contrast(o.emm, method="pairwise", adjust="tukey")
summary(o.pairs)


# Problem 2

prob2 <- read.table(file="512lab09_prob2.txt", header=TRUE)
print(prob2)

prob2$period <- factor(prob2$period)
prob2$subject <- factor(prob2$subject)
prob2$delivery <- factor(prob2$delivery)

boxplot(auc ~ delivery, data=prob2)

o <- lm(auc ~ delivery + period + subject, data=prob2)
anova(o)

# Problem 3

prob3 <- read.table(file="512lab09_prob3.txt", header=TRUE)
print(prob3)

prob3$operator <- factor(prob3$operator)
prob3$noise <- factor(prob3$noise)
prob3$filter <- factor(prob3$filter)

with(prob3, interaction.plot(noise, filter, signal))

o <- lm(signal ~ noise * filter + operator, data=prob3)
anova(o)

oo <- lm(signal ~ noise + filter + operator, data=prob3)
anova(oo)

