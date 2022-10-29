library("glmnet")

logisticRegression <- function(trainingSet, testSet, seed){
  set.seed(seed)
  x = model.matrix(trainingSet$labels ~., data = trainingSet)
  reviews.glmnet <- cv.glmnet(x, trainingSet$labels, family="binomial",type.measure="class")

  print(reviews.glmnet$lambda.1se)
  plot (reviews.glmnet)
  
  newX <- model.matrix(testSet$labels~., data = testSet)
  reviews.logreg.pred <- predict(reviews.glmnet,newx=newX,s="lambda.1se",type="class") 
  print(table(reviews.logreg.pred,testSet$labels))
}
