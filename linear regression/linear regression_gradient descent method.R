#Define MAE_Vector Result
MAE_Vector <- NULL

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
  w <- c(0, 0, 0, 0, 0, 0, 0)
  y <- training$miles_per_gallon
  x <- cbind(training$intercept_term, training$number_of_cylinders, training$displacement,
             training$horsepower, training$weight, training$acceleration, training$model_year)
  costOrigin <- sum(((x%*%w)- y)^2)/(2*trainingNum)
  learningRate <- 0.001
  iterations <- 100000
  for(i in 1:iterations)
  {
    w[1] <- w[1] - learningRate * (1/trainingNum) * sum(((x%*%w)- y)*x[,1])
    w[2] <- w[2] - learningRate * (1/trainingNum) * sum(((x%*%w)- y)*x[,2])
    w[3] <- w[3] - learningRate * (1/trainingNum) * sum(((x%*%w)- y)*x[,3])
    w[4] <- w[4] - learningRate * (1/trainingNum) * sum(((x%*%w)- y)*x[,4])
    w[5] <- w[5] - learningRate * (1/trainingNum) * sum(((x%*%w)- y)*x[,5])
    w[6] <- w[6] - learningRate * (1/trainingNum) * sum(((x%*%w)- y)*x[,6])
    w[7] <- w[7] - learningRate * (1/trainingNum) * sum(((x%*%w)- y)*x[,7])
  }
  costRegressed <- sum(((x%*%w)- y)^2)/(2*trainingNum)
  
  #Test Data Set
  y <- testing$miles_per_gallon
  x <- cbind(testing$intercept_term, testing$number_of_cylinders, testing$displacement,
             testing$horsepower, testing$weight, testing$acceleration, testing$model_year)
  yPredict <- x%*%w
  MAE <- (1/20) * sum(abs(y - yPredict))
  MAE_Vector <- c(MAE_Vector, MAE)
}

#Mean & Standard Deviation
mean <- mean(MAE_Vector)
sd <- sd(MAE_Vector)

