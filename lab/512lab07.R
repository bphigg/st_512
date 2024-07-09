
## ST512 -- Lab 07
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

cotton <- read.table(file="512lab07_prob1.txt", header=TRUE)

# Problem 1

library(emmeans)

cotton$wt <- factor(cotton$weight)
cotton.lm <- lm(strength ~ wt, data=cotton)
cotton.emm <- emmeans(cotton.lm, 'wt')
cotton.pairs <- contrast(cotton.emm, method="pairwise",
                         adjust="bonferroni")
summary(cotton.pairs)

# Problem 2

reading <- read.table(file="512lab07_prob2.txt", header=TRUE)

reading$prg <- factor(reading$prog)
reading$gen <- factor(reading$gender)

with(reading, interaction.plot(prg, gen, words))

reading.lm <- lm(words ~ prg * gen, data=reading)
summary(reading.lm)
reading.lm.red <- lm(words ~ prg + gen, data=reading)
summary(reading.lm.red)
anova(reading.lm.red, reading.lm)

# Problem 3

adhd <- read.table(file="512lab07_prob3.txt", header=TRUE)

adhd$state <- factor(adhd$state)
adhd$trt <- factor(adhd$trt)

with(adhd, interaction.plot(trt, state, activity))

adhd.lm <- lm(activity ~ trt * state, data=adhd)
summary(adhd.lm)
adhd.lm.red <- lm(activity ~ trt + state, data=adhd)
anova(adhd.lm.red, adhd.lm)


