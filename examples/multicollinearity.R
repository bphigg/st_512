
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code to illustrate effects of multicollinearity


# Start with the BAC analysis from before

beer <- read.table(file="data/beer.txt", header=TRUE)
y <- beer$BAC
x <- beer$Beers
plot(x, y)

# Let s control the correlation between x and a new variable z

s <- 0.3
z <- x + 7 + s * rnorm(length(y))

cor(x, z)

o <- lm(y ~ x + z)
summary(o)

plot(x,z)


