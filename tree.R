library(rpart)
library(rpart.plot)

classificationTree <- function(trainingSet, testSet, seed) {
  # rpart uses random sampling so we set the seed to a specific value in order to keep consistent results
  set.seed(seed)
  reviews.rpart <- rpart(labels~., data=trainingSet,cp=0,method="class", minbucket = 1, minsplit = 2 )
  
  # tree with lowest cv error
  
  # deze werd gebruikt door onze makkers van github en kiest de cp waarvoor de cross-val error het laagst is
  # opt.cp <-  reviews.rpart$cptable[which.min(reviews.rpart$cptable[,"xerror"]),"CP"]
  
  # Deze komt van https://stackoverflow.com/questions/37721047/selecting-cp-value-for-decision-tree-pruning-using-rpart
  # en pakt de kleinste tree binnen 1 std error van het minimum
  opt.cp <- cp.select(reviews.rpart)
  
  print(opt.cp)
  plotcp(reviews.rpart)
  printcp(reviews.rpart)
  
  # Prune tree with found cp value
  reviews.rpart.pruned <- prune(reviews.rpart,cp = opt.cp )
  rpart.plot(reviews.rpart.pruned, roundint = FALSE)
  # make predictions on the test set
  reviews.rpart.pred <- predict(reviews.rpart.pruned, newdata=testSet,type="class")
  # show confusion matrix
  print(table(reviews.rpart.pred,test$labels))
}

cp.select <- function(big.tree) {
  min.x <- which.min(big.tree$cptable[, 4]) #column 4 is xerror
  for(i in 1:nrow(big.tree$cptable)) {
    if(big.tree$cptable[i, 4] < big.tree$cptable[min.x, 4] + big.tree$cptable[min.x, 5]) return(big.tree$cptable[i, 1]) #column 5: xstd, column 1: cp 
  }
}
