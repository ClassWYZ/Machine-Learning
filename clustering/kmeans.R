#Author: Wenyu Zhang

#KMeans function
KMeans = function(m, k) {
  #objective function
  l <- NULL
  #d = centroid
  d <- NULL
  for (i in 1 : k) {
    d <- rbind(d, m[i,])
  }
  curr <- vector("numeric", nrow(m))
  
  for (iter in 1 : 20) {
    prev <- curr
    curr <- vector("numeric", nrow(m))
    #updating d (centroid)
    currl <- 0
    for (i in  1 : nrow(m)) {
      mindist <- (m[i,1] - d[1,1])^2 + (m[i,2] - d[1,2])^2
      curr[i] <- 1
      for (j in 2 : k) {
        distance <- (m[i,1] - d[j,1])^2 + (m[i,2] - d[j,2])^2
        if (mindist > distance) {
          mindist <- distance
          curr[i] <- j
        }
      }
      currl <- currl + mindist
    }
    l <- rbind(l, currl)
    #judge coverage
    if (all(prev == curr)) break
    #updating muk
    for (i in 1 : k) {
      indices <- which(curr %in% i)
      muk <- NULL
      for (j in 1 : length(indices)) {
        muk <- rbind(muk, m[indices[j],])
      }
      d[i,1] <- mean(muk[,1])
      d[i,2] <- mean(muk[,2])
    }
  }
  
  if (k == 3) {
    plot(m, asp = 1)
    indices <- which(curr %in% 1)
    points(m[indices,], col = "red")
    indices <- which(curr %in% 2)
    points(m[indices,], col = "green")
    indices <- which(curr %in% 3)
    points(m[indices,], col = "blue")
  }
  
  if (k == 5) {
    plot(m, asp = 1)
    indices <- which(curr %in% 1)
    points(m[indices,], col = "red")
    indices <- which(curr %in% 2)
    points(m[indices,], col = "green")
    indices <- which(curr %in% 3)
    points(m[indices,], col = "blue")
    indices <- which(curr %in% 4)
    points(m[indices,], col = "yellow")
    indices <- which(curr %in% 5)
    points(m[indices,], col = "black")
  }
  
  return(l)
}


#main function
library(mvtnorm)
#number of observations
n = 500
#observations
ob <- NULL
random <- runif(n, 0, 1)
#generate GMM
for (i in 1 : n) {
  if (random[i] <= 0.2) {
    gen <- rmvnorm(1, mean = c(0, 0), sigma = matrix(c(1, 0, 0, 1), nrow = 2))
    ob <- rbind(ob, gen)
  }
  else if (random[i] > 0.2 & random[i] <= 0.7) {
    gen <- rmvnorm(1, mean = c(3, 0), sigma = matrix(c(1, 0, 0, 1), nrow = 2))
    ob <- rbind(ob, gen)
  }
  else {
    gen <- rmvnorm(1, mean = c(0, 3), sigma = matrix(c(1, 0, 0, 1), nrow = 2))
    ob <- rbind(ob, gen)
  }
}
#plot(ob[,1],ob[,2], asp = 1,col=rgb(0,100,0,50,maxColorValue=255), pch=16)
#KMeans
maxIter = 20
ks <- c(2, 3, 4, 5)
m <- as.matrix(ob)
for (k in ks) {
  res <- KMeans(m, k)
  plot(res)
}

