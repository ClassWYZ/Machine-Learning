
distMat = function(testFeat, trainFeat) {
  mat = as.matrix(dist(rbind(testFeat, trainFeat)))
  distMat = mat[1:nrow(testFeat), (nrow(testFeat) + 1):nrow(mat)]
  return(distMat)
}

vecMode = function(vec) {
  candidates = as.numeric(names(table(vec)[table(vec) == max(table(vec))]))
  min(which(vec %in% candidates))
  return(vec[min(which(vec %in% candidates))])
}

kclose = function(distOrder, k, trainLab) {
  kClose = vector()
  for (i in 1:k) {
    kClose = c(kClose, trainLab[distOrder[i]])
  }
  return(vecMode(kClose))
}

knnfunc = function(k, distMat, trainLab) {
  #distMat = eucMat(trainFeat, testFeat) # rows are test set; cols are train set
  matOrder = apply(distMat, MARGIN = 1, FUN = order)
  return(unlist(apply(matOrder, MARGIN = 2, FUN = kclose, k = k, trainLab)))
}


#Read & Split Data
xTrain <- read.csv("Xtrain.txt", header = FALSE)
xLabel <- read.csv("label_train.txt", header = FALSE)$V1
#colnames(xLabel) <- c("label")
yTest <- read.csv("Xtest.txt", header = FALSE)
yLabel <- read.csv("label_test.txt", header = FALSE)$V1
#colnames(yLabel) <- c("label")

digit_distMat = distMat(yTest, xTrain)
digitTest1 = knnfunc(1, digit_distMat, xLabel)
digitTest2 = knnfunc(2, digit_distMat, xLabel)
digitTest3 = knnfunc(3, digit_distMat, xLabel)
digitTest4 = knnfunc(4, digit_distMat, xLabel)
digitTest5 = knnfunc(5, digit_distMat, xLabel)

confusionM1 <- matrix(0, 10, 10)
for (i in 1:500) {
  confusionM1[yLabel[i] + 1, digitTest1[i] + 1] = confusionM1[yLabel[i] + 1, digitTest1[i] + 1] + 1;
}
confusionM2 <- matrix(0, 10, 10)
for (i in 1:500) {
  confusionM2[yLabel[i] + 1, digitTest2[i] + 1] = confusionM2[yLabel[i] + 1, digitTest2[i] + 1] + 1;
}
confusionM3 <- matrix(0, 10, 10)
for (i in 1:500) {
  confusionM3[yLabel[i] + 1, digitTest3[i] + 1] = confusionM3[yLabel[i] + 1, digitTest3[i] + 1] + 1;
}
confusionM4 <- matrix(0, 10, 10)
for (i in 1:500) {
  confusionM4[yLabel[i] + 1, digitTest4[i] + 1] = confusionM4[yLabel[i] + 1, digitTest4[i] + 1] + 1;
}
confusionM5 <- matrix(0, 10, 10)
for (i in 1:500) {
  confusionM5[yLabel[i] + 1, digitTest5[i] + 1] = confusionM5[yLabel[i] + 1, digitTest5[i] + 1] + 1;
}

trace <- rbind(sum(diag(confusionM1)), sum(diag(confusionM2)), sum(diag(confusionM3)), sum(diag(confusionM4)), sum(diag(confusionM5)))
preAccuracy <- trace / 500

q <- read.csv("Q.txt", header = FALSE)
q <- as.matrix(q)
y <- q %*% t(yTest[155,]) #3 examples: 11, 110, 220
y <- matrix(y, nrow = 28, byrow = TRUE)
image(t(apply(y, 2, rev)))
