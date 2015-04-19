#Author: Wenyu Zhang

#KMeans function
KMeans = function(m, k) {
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
    for (i in  1 : nrow(m)) {
      mindist <- sum((m[i,] - d[1,])^2)
      curr[i] <- 1
      for (j in 2 : k) {
        distance <- sum((m[i,] - d[j,])^2)
        if (mindist > distance) {
          mindist <- distance
          curr[i] <- j
        }
      }
    }
    #judge coverage
    if (all(prev == curr)) break
    #updating muk
    for (i in 1 : k) {
      indices <- which(curr %in% i)
      muk <- NULL
      for (j in 1 : length(indices)) {
        muk <- rbind(muk, m[indices[j],])
      }
      for (j in  1 : 20) {
        d[i,j] <- mean(muk[,j])
      }
    }
  }
  
  return(d)
}


library(mvtnorm)

#format: user_id, movie_id, rating
Training <- read.csv("ratings.txt", header = FALSE)
Test<- read.csv("ratings_test.txt", header = FALSE)

#init M matrix
n1 <- max(Training$V1)
n2 <- max(Training$V2)
m <- matrix(0, nrow = max(Training$V1), ncol = max(Training$V2))
for (i in 1 : nrow(Training)) {
  m[Training[i,1], Training[i,2]] = Training[i,3]
}

iter <- 100
lambda <- 10
d <- 20
deviation <- 0.25
#init U (ui: d * 1) and V (vi: d * 1)
I <- diag(d)
Sigma <- lambda^-1 * I
u <- matrix(0, nrow = d, ncol = n1)
v <- matrix(0, nrow = d, ncol = n2)
for (i in 1 : n1) {
  u[,i] <- rmvnorm(1, mean = vector("numeric", d), sigma = Sigma)
}
for (i in 1 : n2) {
  v[,i] <- rmvnorm(1, mean = vector("numeric", d), sigma = Sigma)
}

RMSETotal <- NULL
lTotal <- NULL
#train model
for (t in 1 : iter) {
  #update ui
  for (i in 1 : n1) {
    indices <- which(m[i,] != 0)
    former <- matrix(0, nrow = d, ncol = d)
    latter <- matrix(0, nrow = d, ncol = 1)
    for (j in 1 : length(indices)) {
      former <- former + v[,indices[j]] %*% t(v[,indices[j]])
      latter <- latter + m[i, indices[j]] * v[,indices[j]]
    }
    u[,i] <- solve((lambda * deviation * I) + former) %*% latter
  }
  #update vi
  for (i in 1 : n2) {
    indices <- which(m[,i] != 0)
    former <- matrix(0, nrow = d, ncol = d)
    latter <- matrix(0, nrow = d, ncol = 1)
    if (length(indices) > 0) {
      for (j in 1 : length(indices)) {
        former <- former + u[,indices[j]] %*% t(u[,indices[j]])
        latter <- latter + m[indices[j], i] * u[,indices[j]]
      }
    }  
    v[,i] <- solve((lambda * deviation * I) + former) %*% latter
  }
  
  #predict
  predict <- NULL
  RMSE <- 0
  for (i in 1 : nrow(Test)) {
    tmp <- round(t(u[,Test[i,1]]) %*% v[,Test[i,2]])
    if (tmp > 5) tmp <- 5
    else if (tmp < 1) tmp <- 1
    predict <- rbind(predict, tmp)
    RMSE <- RMSE + (tmp - Test[i,3])^2
  }
  RMSE <- sqrt(RMSE / nrow(Test))
  RMSETotal <- rbind(RMSETotal, RMSE)
  
  #log joint likelihood
  sumu <- 0
  for (i in 1 : n1) {
    sumu <- sumu + sum(u[,i] ^ 2)
  }
  sumv <- 0
  for (i in 1 : n2) {
    sumv <- sumv + sum(v[,i] ^ 2)
  }
  summ <- 0
  for (i in 1 : n1) {
    for (j in 1 : n2) {
      if (m[i,j] != 0) summ <- summ + (m[i,j] - t(u[,i]) %*% v[,j])^2
    }
  }
  l <- 0 - 0.5/deviation * summ - lambda/2 * sumu - lambda/2 * sumv
  lTotal <- rbind(lTotal, l)
}

#q2: three movies
# 5: Copycat (1995); 36: Mad Love (1995); 300: Air Force One (1997)
distv <- vector('numeric', n2)
currentv <- v[,300]#5, 36, 300
for (i in 1 : n2) {
  distv[i] <- sqrt(sum((v[,i] - currentv)^2))
}
which(distv %in% sort(distv)[2:6])

#q3: k-means
centroid <- KMeans(m = t(u), k = 30)
# First 5 centroids
currentu <- as.matrix(centroid[5,])#1, 2, 3, 4, 5
clusterv <- NULL
for (i in 1 : n2) {
  clusterv <- rbind(clusterv, t(currentu) %*% v[,i])
}
which(clusterv %in% sort(clusterv,decreasing = TRUE)[1:10])