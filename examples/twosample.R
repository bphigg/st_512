
##-----------------------------------------------------------------------------
## ST512 -- R codes for review of two-sample testing
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
##-----------------------------------------------------------------------------

# Data from Example 4.8 in Rao's "Statistical Research Methods"
# Goal is to compare chemicals collected at two river positions (up and downstream) 

# Data entry

up <- c(24.5, 29.7, 20.4, 28.5, 25.3, 21.8, 20.2, 21.0, 21.9, 22.2)
down <- c(32.8, 30.4, 32.3, 26.4, 27.8, 26.9, 29.0, 31.5, 31.2, 26.7, 25.6, 25.1, 32.8, 34.3, 35.4)

# First graphical look

op <- par(pty="m", mfrow=c(2, 2))
hist(up, col="gray", border="white")
hist(down, col="gray", border="white")
plot(density(up), col="gray", main="PDFs: up vs. down", xlim=c(15, 43))
lines(density(down))
boxplot(list(up, down), names=c("up", "down"), col="gray", main="Boxplot of up vs. down")
par(op)

# First numerical look

sapply(list(up=up, down=down), summary)
sapply(list(up=up, down=down), sd)

# If we accept a normality assumption, can use t-test to compare means

out.t <- t.test(x=up, y=down, var.equal=TRUE)
print(out.t)

# ADVANCED: If we don't accept normality, then we can use a permutation-based method

M <- 5000
d <- c(up, down)
n.up <- length(up)
n.down <- length(down)
N <- length(d)
t.perm <- numeric(M)
for(m in 1:M) {
  
  id <- sample(N, size=n.up, replace=FALSE)
  new.up <- d[id]
  new.down <- d[-id]
  t.perm[m] <- t.test(x=new.up, y=new.down)$statistic
  
}
hist(t.perm, freq=FALSE, col="gray", border="white")
abline(v=out.t$statistic, col=2)
perm.p <- mean(abs(t.perm) >= abs(out.t$statistic))
print(perm.p)
curve(dt(x, df=23), add=TRUE, col="blue")


