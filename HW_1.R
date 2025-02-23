
## ST 512
## Homework 1
## Brian Higginbotham

## 11.22

treadmill <- read.table(file="data/fitness.txt", header=TRUE)
treadmill

# a
plot(treadmill$treadmill, treadmill$tenk)

# b
slr_tread <- lm(tenk ~ treadmill, data=treadmill)
abline(slr_tread, col=2)
summary(slr_tread)

# c
# y_hat = 58.8158 -1.8673(x_i)

## 11.23
#a
#Residual Standard Error = sqrt(residual variance)
RSE <- 2.102 # from slr_tread summary
#estimated residual variance:
RSE**2

#b
# estimate the standard error for b_hat_1
# 0.3462 - summary output

#c
# place a 95% CI on b_hat_1
confint(slr_tread, 'treadmill', level=0.90)

#d
# hypothesis test that there is a linear relationship between treadmill time and 10K time
# Pr(>|t|) for 'treadmill' from summary output
# 3.99e-05 - near zero percent chance of the null hypothesis (b_hat_1 = 0) occuring for the t value 
# -5.393 in the student t-distribution. Reject H_0 - conclude there is a linear relationship between 
# treadmill time and 10K time

y_hat <- 58.8158 - 1.8673*11
y_hat


predict(slr_tread, data.frame(treadmill=11), interval="conf", level =0.95)

predict(slr_tread, data.frame(treadmill=11), interval="pred", level =0.95)


drug <- read.table(file="data/drug.txt", header=TRUE)
drug
y<-drug$response
x<-drug$dose
plot(x,y)
slr_drug <- lm(y ~ x)
summary(slr_drug)
abline(slr_drug, col=4)
slr_drug$residuals

plot(slr_drug$fitted.values, slr_drug$residuals)
abline(h=0, col=6)
plot(x, slr_drug$residuals)
abline(h=0, col=5)
qqnorm(slr_drug$residuals); qqline(slr_drug$residuals)

# 11.42
xi<-log(x)
xi
plot(xi,y)
slr_drug <- lm(y ~ xi)
summary(slr_drug)
abline(slr_drug, col=4)

plot(slr_drug$fitted.values, slr_drug$residuals)
abline(h=0, col=6)
plot(xi, slr_drug$residuals)
abline(h=0, col=5)
qqnorm(slr_drug$residuals); qqline(slr_drug$residuals)