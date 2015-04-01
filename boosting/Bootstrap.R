#Part1 function: bootstrap
bootstrap = function(w, n) {
  sample <- vector("integer", n)
  cdf <- vector("numeric", length(w))
  tmp <- vector("numeric", length(w))
  for (i in 1:length(w)) {
    tmp[i] <- 1
    cdf[i] <- w %*% tmp
  }
  random <- runif(n, 0 ,1)
  for (i in 1:n) {
    for (j in 1:length(w)) {
      if (cdf[j] >= random[i]) {
        sample[i] = j
        break
      }
    }
  }
  return(sample)
}


#Read & Split Data
AllData <- read.csv("X.csv", header = FALSE)
AllLabel<- read.csv("y.csv", header = FALSE)

xTrain <- AllData[1:183,]
xLabel <- AllLabel[1:183,]
#colnames(xLabel) <- c("label")
yTest <- AllData[184:683,]
yLabel <- AllLabel[184:683,]
#colnames(yLabel) <- c("label")

#Part1 run by bootstap(w, n)
w <- c(0.1, 0.2, 0.3, 0.4)
dataset <- bootstrap(w, 500) #choose n to be 100, 200, 300, 400, 500
hist(dataset, breaks=seq(min(dataset)-0.5, max(dataset)+0.5, by=1))
