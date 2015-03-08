#xi = row vector; w = row vector
bern = function(xi, w) {
  tmp <- exp(as.matrix(xi) %*% as.matrix(w))
  bern <- tmp / (1 + tmp)
  return(bern)
}
##yi
berntotal = function(i, j, xTrain, w) {
  berntotal <- ifelse((500 * (i - 1) < j) && (j < 500 * i + 1), bern(xTrain[j,], w[i,]), 1 - bern(xTrain[j,], w[i,]))
  return(berntotal);
}
##L
ml = function(xTrain, bernValue, i) {
  ml <- 0
  for (j in 1:nrow(xTrain)) {
    ml <- ml + log(ifelse((500 * (i - 1) < j) && (j < 500 * i + 1), bernValue[j,i], 1 - bernValue[j,i]), exp(1))
  }
  return(ml)
}

#Read & Split Data
xTrain <- read.csv("Xtrain.txt", header = FALSE)
xLabel <- read.csv("label_train.txt", header = FALSE)$V1
#colnames(xLabel) <- c("label")
yTest <- read.csv("Xtest.txt", header = FALSE)
yLabel <- read.csv("label_test.txt", header = FALSE)$V1
#colnames(yLabel) <- c("label")

learningRate <- 0.1/5000
iterations <- 1000
k <- 10

xTrainC <- list()
for (i in 1:10) {
  xTrainC[[i]] <- xTrain[(500*(i-1)+1):(500*i),]
}
w <- matrix(0, 10, 20)
l <- NULL
for (iter in 1:iterations) {
  l <- cbind(l, c(0,0,0,0,0,0,0,0,0,0))
  for (i in 1:10) {
    gradient <- matrix(0, 1, 20)
    bernValue <- bern(xTrain, t(w))
    gradient <- (-1) * t(as.matrix(bernValue[,i])) %*% as.matrix(xTrain)
    for (j in (500*(i-1) + 1):(500*i)) {
      gradient <- gradient + as.vector(xTrain[j,])
    }
    w[i,] <- w[i,] + learningRate * as.matrix(gradient)
    l[i,iter] <- ml(xTrain, bernValue, i)
  }
}

digitTest = NULL
for (i in 1:nrow(yTest)) {
  test <- NULL
  for (j in 1:10) {
    test <- rbind(test, bern(yTest[i,], w[j,]))
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

###q2
plot(l[1,],type="l",ylim=c(min(l),max(l)),col=26,lty=1,ylab="L",lwd=2,xlab="Iteration")
lines(l[2,],type="l",col=33,lty=1)
lines(l[3,],type="l",col=36,lty=1)
lines(l[4,],type="l",col=43,lty=1)
lines(l[5,],type="l",col=47,lty=1)
lines(l[6,],type="l",col=53,lty=1)
lines(l[7,],type="l",col=68,lty=1)
lines(l[8,],type="l",col=84,lty=1)
lines(l[9,],type="l",col=152,lty=1)
lines(l[10,],type="l",col=153,lty=1)
grid()
legend( x="bottomright", 
        legend=c(expression(w[0]), expression(w[1]), expression(w[2]), expression(w[3]), expression(w[4]), 
                 expression(w[5]), expression(w[6]), expression(w[7]), expression(w[8]), expression(w[9])),
        col=c(26, 33, 36, 43, 47, 53, 68, 84, 152, 153), lwd=1, lty=c(1), 
        pch=c(NA,NA) )

###q4
q <- read.csv("Q.txt", header = FALSE)
q <- as.matrix(q)
y <- q %*% t(yTest[304,]) #3 examples: 106, 155, 304
y <- matrix(y, nrow = 28, byrow = TRUE)
image(t(apply(y, 2, rev)))
