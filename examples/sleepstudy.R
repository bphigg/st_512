## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for analysis of the "sleepstudy" data

library(lme4)
data(sleepstudy)
print(sleepstudy)

ids <- rep(1:18, each=10)

plot(Reaction ~ Days, data=sleepstudy, col=ids)
abline(lm(Reaction ~ Days, data=sleepstudy), lwd=2)

plot(Reaction ~ Days, type="n", data=sleepstudy)
iids <- unique(sleepstudy$Subject)
for(j in seq_along(iids)) {
  
  o.lm <- lm(Reaction ~ Days, data=sleepstudy, subset=Subject==iids[j])
  abline(coef(o.lm), col=j)
  
}

o.lmer <- lmer(Reaction ~ Days + (1 + Days | Subject), data=sleepstudy)
summary(o.lmer)

oo.lmer <- lmer(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), data=sleepstudy)
summary(oo.lmer)
anova(oo.lmer, o.lmer)

ooo.lmer <- lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
summary(ooo.lmer)
anova(ooo.lmer, oo.lmer)

# Visual summary of oo.lmer model fit

ab.fix <- fixef(oo.lmer)
ab.ran <- ranef(oo.lmer)$Subject

plot(Reaction ~ Days, data=sleepstudy, col=ids)
abline(lm(Reaction ~ Days, data=sleepstudy), lwd=2)

for(j in seq_along(ab.ran[,1])) {
  
  a <- ab.fix[1] + ab.ran[j,1]
  b <- ab.fix[2] + ab.ran[j,2]
  abline(a=a, b=b , col=j)
  
}

