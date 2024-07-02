
## ST 512
## Homework 1
## Brian Higginbotham

## 11.22

treadmill <- read.table(file="data/fitness.txt", header=TRUE)
treadmill

# a
plot(treadmill$treadmill, treadmill$tenk)

# b
slr_tread <- lm(treadmill$tenk ~ treadmill$treadmill)
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
