
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Exercise 12.42 in Ott & Longnecker

dp <- read.table(file="../Data/exercise_12-42.txt", header=TRUE)
print(dp)

dp$agg <- factor(dp$agg)
dp$race <- factor(dp$race)

xt <- xtabs(cbind(yes, no) ~ race, data=dp)
print(xt)
summary(xt)

o <- glm(cbind(yes, no) ~ agg * race, family=binomial, data=dp)
summary(o)
oo <- glm(cbind(yes, no) ~ agg + race, family=binomial, data=dp)
anova(oo, o)
print(pval <- 1 - pchisq(2.23, df=5))
summary(oo)
ooo <- glm(cbind(yes, no) ~ race, family=binomial, data=dp)
summary(ooo)
anova(ooo, oo)

data.frame(agg=dp$agg, race=dp$race, prop=dp$yes / (dp$yes + dp$no), 
           fitted=predict(oo, type="response"))

