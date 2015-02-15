#p Function
pFunc <- NULL

#p Difference Matrix
pDiffMatrix <- NULL

#Calculate p 1-4
for (pExp in 1:4) {
  #Define pDiff for each p to store (ytest - ypredict) results
  pDiff <- NULL
  #Define RMSE_Vector Result
  RMSE_Vector <- NULL
  for (t in 1:1000) {
    
    #Read & Split Data
    y <- read.csv("y.txt", header = FALSE)
    colnames(y) <- c("miles_per_gallon")
    x <- read.csv("x.txt", header = FALSE)
    colnames(x) <- c("intercept_term", "number_of_cylinders", "displacement", "horsepower",
                     "weight", "acceleration", "model_year")
    sampleRow <- sample(c(1:392), 20)
    dataset <- cbind(y, x)
    #dataset <- as.matrix(dataset)
    testing = NULL
    training = NULL
    for (rowNum in 1:392 ){
      if (any(sampleRow == rowNum))
        testing <- rbind(testing, dataset[rowNum,])
      else
        training <- rbind(training, dataset[rowNum,])
    }
    
    #Train Data Set
    trainingNum <- nrow(training)
    y <- training$miles_per_gallon
    x <- cbind(training$intercept_term)
    for (e in 1:pExp) {
      x <- cbind(x, training$number_of_cylinders^e, training$displacement^e,
                 training$horsepower^e, training$weight^e, training$acceleration^e, training$model_year^e)
    }
    #y <- as.matrix(y)
    #x <- as.matrix(x)
    w <- solve(t(x) %*% x) %*% t(x) %*% y
    
    #Test Data Set
    y <- testing$miles_per_gallon
    x <- cbind(testing$intercept_term)
    for (e in 1:pExp) {
      x <- cbind(x, testing$number_of_cylinders^e, testing$displacement^e,
                 testing$horsepower^e, testing$weight^e, testing$acceleration^e, testing$model_year^e)
    }
    yPredict <- x %*% w
    yDiff <- y - yPredict
    pDiff <- cbind(t(yDiff), pDiff)
    RMSE <- sqrt((1/20) * sum((y - yPredict)^2))
    RMSE_Vector <- c(RMSE_Vector, RMSE)
  }
  
  #Mean & Standard Deviation
  mean <- mean(RMSE_Vector)
  sd <- sd(RMSE_Vector)
  p <- c(mean, sd)
  pFunc <- rbind(pFunc, p)
  pDiffMatrix <- rbind(pDiffMatrix, pDiff)
}
row.names(pFunc) <- c("p1","p2", "p3", "p4")
#row.names(pDiffMatrix) <- c("p1","p2", "p3", "p4")
hist(pDiffMatrix[1,])
hist(pDiffMatrix[2,])
hist(pDiffMatrix[3,])
hist(pDiffMatrix[4,])

#ML fit for Gaussian
pGauss <- NULL
for (pExp in 1:4) {
  m <- mean(pDiffMatrix[pExp,])
  s <- sd(pDiffMatrix[pExp,])^2
  l <- (-20000)*log(sqrt(2 * pi * s)) - 10000
  pGauss <- rbind(pGauss, c(m, s, l))
}
