meanTrain = function(m) {
  meanTrain <- vector("numeric",20)
  m <- as.matrix(m)
  for (i in 1:ncol(m)) meanTrain[i] <- mean(m[,i])
  return(meanTrain)
}
covTrain = function(m, mean) {
  covTrain <- matrix(0, 20, 20)
  m <- as.matrix(m)
  #mean <- as.vector(mean)
  for (i in 1:nrow(m)) covTrain <- covTrain + ((m[i,] - mean) %*% t((m[i,] - mean)))
  covTrain <- covTrain/(nrow(m))
  return(covTrain)
}
#prior
digitPrior = function(cat, mean, sd) {
  digitPrior = (1/(sqrt(2*pi)*sd)) * exp((-1)*((cat - mean)^2)/(2*sd^2))
  return(digitPrior)
}

#data likelihood
mutiGass = function(x, cov, mean) {
  mutiGass <- (1/((2*pi)^10 * sqrt(det(cov)))) * exp((-0.5) * as.matrix(x - mean) %*% solve(cov) %*% t(as.matrix(x - mean))) 
  return(mutiGass)
}

#Read & Split Data
xTrain <- read.csv("Xtrain.txt", header = FALSE)
xLabel <- read.csv("label_train.txt", header = FALSE)$V1
#colnames(xLabel) <- c("label")
yTest <- read.csv("Xtest.txt", header = FALSE)
yLabel <- read.csv("label_test.txt", header = FALSE)$V1
#colnames(yLabel) <- c("label")

## calculate mean and sd of xLabel
sd <- sd(xLabel)
mean <-mean(xLabel)

xTrainC <- list()
for (i in 1:10) {
  xTrainC[[i]] <- xTrain[(500*(i-1)+1):(500*i),]
}
xTrainMean <- NULL
for (i in 1:10) {
  xTrainMean <- rbind(xTrainMean, meanTrain(xTrainC[[i]]))
}
xTrainCov <- list()
for (i in 1:10) {
  xTrainCov[[i]] <- covTrain(xTrainC[[i]],xTrainMean[i,])
}

digitTest = NULL
for (i in 1:nrow(yTest)) {
  test <- NULL
  for (j in 1:10) {
    test <- rbind(test, digitPrior(j-1, mean, sd) * mutiGass(yTest[i,], xTrainCov[[j]], xTrainMean[j,]))
  }
  index <- 0
  max <- 0
  for (j in 1:10) {
    if(test[j] > max) {
      index <- j
      max <- test[j]
    }
  }
  digitTest <- rbind(digitTest, index - 1)
}

###q3
confusion <- matrix(0, 10, 10)
for (i in 1:500) {
  confusion[yLabel[i] + 1, digitTest[i] + 1] = confusion[yLabel[i] + 1, digitTest[i] + 1] + 1;
}

trace <- sum(diag(confusion))
preAccuracy <- trace / 500

###q4
q <- read.csv("Q.txt", header = FALSE)
q <- as.matrix(q)
y <- q %*% as.matrix(xTrainMean[10,]) #1,2...,10
y <- matrix(y, nrow = 28, byrow = TRUE)
image(t(apply(y, 2, rev)))

###q5
q <- read.csv("Q.txt", header = FALSE)
q <- as.matrix(q)
y <- q %*% t(yTest[11,]) #3 examples: 11, 85, 141
y <- matrix(y, nrow = 28, byrow = TRUE)
image(t(apply(y, 2, rev)))
