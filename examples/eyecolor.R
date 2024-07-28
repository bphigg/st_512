
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for eye (and hair) color example in Faraway's book

library(faraway) 

data(haireye)
?haireye
print(haireye)

# One-way table, eye color only

eye.tab <- xtabs(y ~ eye, data=haireye)
print(eye.tab)

o.eye <- glm(y ~ eye, family=poisson, data=haireye)
summary(o.eye)
anova(o.eye)
print(1 - pchisq(312.04, df=12))

# Two-way table, eye and hair color combos

haireye.tab <- xtabs(y ~ hair + eye, data=haireye)
print(haireye.tab)
summary(haireye.tab) 

o.haireye <- glm(y ~ hair + eye, family=poisson, data=haireye)
summary(o.haireye)

oo.haireye <- glm(y ~ hair * eye, family=poisson, data=haireye)
summary(oo.haireye)

a <- anova(o.haireye, oo.haireye); print(a)
print(1 - pchisq(a$Deviance[2], df=a$Df[2]))
