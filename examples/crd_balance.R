
## ST512
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code generate a BALANCED completely randomized design

# This is super simple -- just randomly permute unit labels and then group them

get.crd.balanced <- function(nlevels, nrep) {
  
  N <- nlevels * nrep
  o <- sample(N)
  M <- matrix(o, nrow=nrep)
  colnames(M) <- paste("level", 1:nlevels, sep="")
  rownames(M) <- paste("rep", 1:nrep, sep="")
  return(M)
  
}

# Example: Treatment with 4 levels and 5 replications each

get.crd.balanced(4, 5)
