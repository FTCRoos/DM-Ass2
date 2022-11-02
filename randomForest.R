library(randomForest)

classificationRandomForest <- function (trainingSet, testSet, seed){
  set.seed(seed)
  
  trainingSet$labels = as.factor(trainingSet$labels)
  trainingSet.nolabels <- subset(trainingSet, select = -c(labels) )
  
  testSet$labels = as.factor(testSet$labels)
  
  OOB.matrix <- tuneRF(trainingSet.nolabels, as.factor(trainingSet$labels),
                                ntreeTry = 500, doBest = FALSE, plot = TRUE)
  print(OOB.matrix)
  optimal.mtry <- OOB.matrix[which.min(OOB.matrix[,2]),1]
  print(optimal.mtry)
  classifier <- randomForest(as.factor(labels) ~ ., data=as.matrix(trainingSet),
                             ntree = 1000, mtry = optimal.mtry, type = "classification", err.rate = TRUE)
  err.rate <- cbind( c(1:1000),classifier$err.rate[,1])

  optimal.ntree <- err.rate[which.min(err.rate[,2]), 1]
  print(optimal.ntree)
  colnames(err.rate) <- c( "ntree","OOB error")
  plot(err.rate)
  classifier <- randomForest(as.factor(labels) ~ ., data=as.matrix(trainingSet), mtry = optimal.mtry,
                                      ntree = optimal.ntree, type = "classification")
  # Predicting the Test set results
  newX <- model.matrix(testSet$labels~., data = testSet)
  random.forests.predictions <- predict(classifier, newdata = newX)
  random.forests.predictions.table <- table(random.forests.predictions,testSet$labels)
  getScore(random.forests.predictions.table)
  return (random.forests.predictions)

}
