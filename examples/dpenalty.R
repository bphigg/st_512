
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Three-way Poisson model
# Race and death penalty example
# Data from study published in 1991 Florida Law Review

V <- as.factor(c('w', 'w', 'w', 'w', 'b', 'b', 'b', 'b')) # victim's race
D <- as.factor(c('w', 'w', 'b', 'b', 'w', 'w', 'b', 'b')) # defendant's race
P <- as.factor(c('y', 'n', 'y', 'n', 'y', 'n', 'y', 'n')) # death penalty
y <- c(53, 414, 11, 37, 0, 16, 4, 139)

xtabs(y ~ D + P + V)

# Full model, all interactions
o1 <- glm(y ~ D * P * V, family=poisson) 
summary(o1)
anova(o1)

# conditional independence: D & P independent, given V
o2 <- glm(y ~ D * V + P * V, family=poisson) 
summary(o2)

# mutual independence
o3 <- glm(y ~ D + P + V, family=poisson) 
summary(o3)

anova(o2, o1)
print(1 - pchisq(5.39, df=2))
anova(o3, o2)

# Simpson's paradox

marg <- xtabs(y ~ D + P)
print(marg)
marg[2,2] / marg[2,1] ## odds of P=y for D=w
marg[1,2] / marg[1,1] ## odds of P=y for D=b

full <- xtabs(y ~ D + P + V)
print(full)
full[2,2,2] / full[2,1,2] ## odds of P=y for D=w | V=w
full[1,2,2] / full[1,1,2] ## odds of P=y for D=b | V=w
full[2,2,1] / full[2,1,1] ## odds of P=y for D=w | V=b
full[1,2,1] / full[1,1,1] ## odds of P=y for D=b | V=b

