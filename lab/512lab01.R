
## ST512 -- Lab 01
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)

# Part 1: Read in data 

grapes <- read.table(file="grapes.txt", header=TRUE)
nclust <- grapes$nclust
yield <- grapes$yield

# Part 2: Basic numerical summaries

mean(nclust)
mean(yield)

sd(nclust)
sd(yield)

summary(nclust)
summary(yield)

t.test(yield)

hist(nclust)
hist(yield)

# Part 3: Simple linear regression

plot(x=nclust, y=yield)
out <- lm(yield ~ nclust)
abline(out, col=2)
summary(out)
predict(out, data.frame(nclust=102), interval="conf")
plot(nclust, out$residuals); abline(h=0, lty=3)

