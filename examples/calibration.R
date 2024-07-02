
## ST512
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code to help calibrate expectations about diagnostic plots

sim.diag.plots <- function(n) {
  
  x <- runif(n, -1, 1)
  y <- 1 + 2 * x + rnorm(n)
  o <- lm(y ~ x)
  e <- o$residuals
  yhat <- o$fitted.values
  op <- par(pty="m", mfrow=c(2, 2))
  plot(x=yhat, y=e, xlab="y.hat", ylab="e"); abline(h=0, lty=3)
  plot(e); abline(h=0, lty=3)
  plot(x=x, y=e); abline(h=0, lty=3)
  qqnorm(e, main=""); qqline(e)
  par(op)
  
}

# repeat the following code over and over, see variations

sim.diag.plots(25)

