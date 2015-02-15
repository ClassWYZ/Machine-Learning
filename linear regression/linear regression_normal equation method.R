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
  w <- solve(t(x) %*% x) %*% t(x) %*% y
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

