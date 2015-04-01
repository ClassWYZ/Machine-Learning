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

#logistic: return a 10*1 vector w
logistic = function(m, step) {
  #initialize w
  w <- vector("numeric", 10)
  w <- as.matrix(w)
  #randomize order
  sample <- m[sample(1:nrow(m), nrow(m),replace=FALSE),]
  for (i in 1:nrow(m)) {
    yi <- m[i,11]
    xi <- as.matrix(m[i, 1:10])
    w <- w + step * as.numeric(1 - (1 / (1 + exp(-yi * t(xi) %*% w)))) * yi * xi
  }
  return(w)
}
classifier = function(x, w) {
  f <- x %*% w
  f <- sign(f)
  return (f)
}

boostclassifier = function(x, alpha, w) {
  f <- 0
  for (i in 1:nrow(alpha)) {
    f <- f + alpha[i] * classifier(x, w[,i])
  }
  f <- sign(f)
  return (f)
}

#Read & Split Data
AllData <- read.csv("X.csv", header = FALSE)
AllLabel<- read.csv("y.csv", header = FALSE)

yTest <- AllData[1:183,]
yLabel <- AllLabel[1:183,]
#colnames(xLabel) <- c("label")
xTrain <- AllData[184:683,]
xLabel <- AllLabel[184:683,]
#colnames(yLabel) <- c("label")
xTrainLabeled <- cbind(xTrain, xLabel)
xTrainLabeled <- as.matrix(xTrainLabeled)
xTrain <- as.matrix(xTrain)
yTest <- as.matrix(yTest)

step <- 0.1
p <- rep(1, 500)
p <- p / length(p)
wTotal <- NULL
error <- vector("numeric", 1000)
trainErr <- vector("numeric", 1000)
testErr <- vector("numeric", 1000)
p1 <- vector("numeric", 1000)
p2 <- vector("numeric", 1000)
p3 <- vector("numeric", 1000)
boostingTrain <- vector("numeric", 500)
boostingTest <- vector("numeric", 183)
#alpha <- vector("numeric", 1000) 
alpha <- NULL
for (t in 1:1000) {
  View(t)
  p1[t] = p[10]
  p2[t] = p[122]
  p3[t] = p[433]
  dataset <- bootstrap(p, 500)
  #hist(dataset, breaks=seq(min(dataset)-0.5, max(dataset)+0.5, by=1))
  train <- NULL
  for (i in 1:500) {
    train <- rbind(train, xTrainLabeled[dataset[i],])
  }
  #  while (sum(train[,10]) == 500) {
  #    dataset <- bootstrap(p, 500)
  #    for (i in 1:500) {
  #      train <- rbind(train, xTrainLabeled[dataset[i],])
  #    }
  #  }
  result <- logistic(train, step)
  wTotal <- cbind(wTotal, result)
  for (i in 1:500) {
    if (classifier(xTrain[i,], result) != xLabel[i]) error[t] <- error[t] + p[i]
  }
  #alpha[t] <- 1/2 * log((1-error[t])/error[t])
  alpha <- rbind(alpha, 1/2 * log((1-error[t])/error[t]))
  for (i in 1:500) {
    p[i] <- p[i] * exp(-alpha[t] * xLabel[i] * classifier(xTrain[i,], result))
  }
  p <- p / sum(p)
  ##training and testing error
  #wcurrent =  wTotal %*% alpha
  for (i in 1:500) {
    boostingTrain[i] <- boostingTrain[i] + alpha[t] * classifier(xTrain[i,], wTotal[,t])
  }
  for (i in 1:183) {
    boostingTest[i] <- boostingTest[i] + alpha[t] * classifier(yTest[i,], wTotal[,t])
  }
  for (i in 1:500) {
    #if (boostclassifier(xTrain[i,],alpha,wTotal) != xLabel[i]) trainErr[t] <- trainErr[t] + 1
    if (sign(boostingTrain[i]) != xLabel[i]) trainErr[t] <- trainErr[t] + 1
  }
  trainErr[t] <- trainErr[t] / 500
  for (i in 1:183) {
    if (sign(boostingTest[i])!= yLabel[i]) testErr[t] <- testErr[t] + 1
  }
  testErr[t] <- testErr[t] / 183
  ##error function end
}

##q2 plot trainErr and testErr
plot(testErr, type='l', ylim=c(0.0,0.2), xlab='T', ylab='Error')
par(new=T)
plot(trainErr, type='l',col="blue", ylim=c(0.0,0.2), xlab='', ylab='', axes=F)
grid()
legend( x="topright", 
        legend=c(expression(TestError), expression(TrainError)),
        col=c(33, 36), lwd=1, lty=c(1), 
        pch=c(NA,NA) )

##q3 bayes without boosting
#nonboosting <- NULL
#nonboosting <- bayes(xTrainLabeled[,2:11])
#nonboostingtestErr <- 0
#for (i in 1:183) {
#  if ((as.matrix(yTest[i,]) %*% nonboosting * yLabel[i]) < 0) nonboostingtestErr <- nonboostingtestErr + 1
#}
#nonboostingtestErr <- nonboostingtestErr / 183

##q4 plot error and alpha
plot.ts(error)
lines(alpha, col = 36)
grid()
legend( x="topright", 
        legend=c(expression(error), expression(alpha)),
        col=c(33, 36), lwd=1, lty=c(1), 
        pch=c(NA,NA) )

##q5 plot p1 p2 p3
plot.ts(p1)
plot.ts(p2)
plot.ts(p3)
#lines(p2, col = 36)
#lines(p3, col = 26)
#grid()
#legend( x="topright", 
#        legend=c(expression(p1), expression(p2), expression(p3)),
#        col=c(33, 36, 26), lwd=1, lty=c(1), 
#        pch=c(NA,NA) )

