#Author: Wenyu Zhang

#library(mvtnorm)

data <- read.csv("faces.csv", header = FALSE)

#part1 of Problem2
iter <- 200
k <- 25
x <- as.matrix(data)
dx <- dim(x)
n <- dx[1]
m <- dx[2]
W <- matrix(abs(rnorm(n * k)), n, k)
H <- matrix(abs(rnorm(k * m)), k, m)
Obj <- NULL

for (t in 1 : iter) {
  H = H * ((t(W) %*% x) / ((t(W) %*% W) %*% H))
  W = W * ((x %*% t(H)) / (W %*% (H %*% t(H))))
  s <- sum((x - W %*% H)^2)
  Obj <- rbind(Obj, s)
}

plot(Obj)

#image: 50
#top = 12
image <- x[,50]
y <- matrix(image, nrow = 32, byrow = FALSE)
image(t(apply(y, 2, rev)))
h <- H[,50]
top <- which(h %in% sort(h, decreasing = TRUE)[1:1])
image2 <- W[,top]
y2 <- matrix(image2, nrow = 32, byrow = FALSE)
image(t(apply(y2, 2, rev)))

#image: 500
#top = 25
image <- x[,500]
y <- matrix(image, nrow = 32, byrow = FALSE)
image(t(apply(y, 2, rev)))
h <- H[,500]
top <- which(h %in% sort(h, decreasing = TRUE)[1:1])
image2 <- W[,top]
y2 <- matrix(image2, nrow = 32, byrow = FALSE)
image(t(apply(y2, 2, rev)))

#image: 1000
#top = 20
image <- x[,1000]
y <- matrix(image, nrow = 32, byrow = FALSE)
image(t(apply(y, 2, rev)))
h <- H[,1000]
top <- which(h %in% sort(h, decreasing = TRUE)[1:1])
image2 <- W[,top]
y2 <- matrix(image2, nrow = 32, byrow = FALSE)
image(t(apply(y2, 2, rev)))




