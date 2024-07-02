
## ST512
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code to plot and compare confidence & prediction intervals

pi.ci.plot <- function(x, y) {
  
  rng <- diff(range(x))
  xmin <- min(x) - 0.1 * rng
  xmax <- max(x) + 0.1 * rng
  xx <- seq(xmin, xmax, length=100)
  new <- data.frame(x=xx)
  out <- lm(y ~ x)
  p.int <- predict(out, new, interval="pred")
  c.int <- predict(out, new, interval="conf")
  ylim <- range(c(p.int[,2:3]))
  plot(0, 0, type="n", xlim=c(xmin, xmax), ylim=ylim, xlab="x", ylab="y")
  polygon(c(xx, rev(xx)), c(p.int[,2], rev(p.int[,3])), col="lightgray", border=NA)
  polygon(c(xx, rev(xx)), c(c.int[,2], rev(c.int[,3])), col="darkgray", border=NA)
  points(x, y)
  abline(out)
  
}

# Example 11.4 in Ott & Longnecker 

x <- c(3.3, 3.4, 3.4, 3.5, 3.6, 3.6, 3.7, 3.7, 3.8, 3.8, 3.9, 4.0, 4.1, 4.2, 4.3, 
       4.4, 4.5, 5.0, 5.1, 5.2)
y <- c(17.78, 21.59, 23.84, 15.13, 23.45, 20.87, 17.78, 20.09, 17.78, 12.46, 14.95, 
       15.87, 17.45, 14.35, 14.64, 17.25, 12.57, 7.15, 7.50, 4.34)
out <- lm(y ~ x)
pi.ci.plot(x, y)

