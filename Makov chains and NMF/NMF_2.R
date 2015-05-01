#Author: Wenyu Zhang

#library(mvtnorm)

#data preprocess
data <- read.table("nyt_data.txt", header = FALSE)
m <- matrix(0, 3012, 8447)
data <- as.matrix(data)
for (i in 1 : nrow(data)) {
  tmp <- strsplit(data[i,], ",")
  l <- length(tmp[[1]])
  for (j in 1 : l) {
    curr <- tmp[[1]][j]
    s <- strsplit(curr, ":")
    index <- as.numeric(s[[1]][1])
    number <- as.numeric(s[[1]][2])
    m[index, i] <- m[index, i] + number
  }
}

#part2 of Problem2
iter <- 200
k <- 25
eps <- 10^-16
x <- m
dx <- dim(x)
n <- dx[1]
m <- dx[2]
W <- matrix(abs(rnorm(n * k)), n, k)
H <- matrix(abs(rnorm(k * m)), k, m)
Obj <- NULL
#W = (W./(repmat(sum(H, 2)', p, 1))).*((X./(W*H))*H');
for (t in 1 : iter) {
  sumW <- colSums(W)
  sumW <- as.matrix(sumW)
  sumM <- matrix(sumW, length(sumW), m, byrow = FALSE)
  H <- (H / (sumM + eps)) * (t(W) %*% (x / (W %*% H + eps)))
  sumH <- rowSums(H)
  sumH <- t(sumH)
  sumM <- matrix(sumH, n, length(sumH), byrow = TRUE)
  W <- (W / (sumM + eps)) * ((x / (W %*% H + eps)) %*% t(H))
  s <- sum(x * (log(1/(W %*% H + eps))) + (W %*% H))
  Obj <- rbind(Obj, s)
}

plot(Obj)

#q3
for (i in 1 : k) {
  W[, i] <- W[, i] / sum(W[, i]) 
}
#5 columns: 3,6,8,11,16
name <- read.table("nytvocab.dat", header = FALSE)
name <- as.matrix(name)
w <- W[, 6]
top <- which(w %in% sort(w, decreasing = TRUE)[1:10])
topname <- NULL
for (i in 1 : 10) {
  topname <- rbind(topname, name[top[i]])
}
topprop <- NULL
for (i in 1 : 10) {
  topprop <- rbind(topprop, w[top[i]])
}
final <- cbind(topname, topprop)
