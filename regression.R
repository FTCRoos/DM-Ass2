library("glmnet")

logisticRegression <- function(trainingSet, testSet, seed){
  set.seed(seed)
  x = model.matrix(trainingSet$labels ~., data = trainingSet)
  reviews.glmnet <- cv.glmnet(x, trainingSet$labels,
                                       family="multinomial",type.measure="class")
  print(coef(reviews.glmnet,s="lambda.min"))
  print(reviews.glmnet$lambda.min)
  plot (reviews.glmnet)
  newX <- model.matrix(testSet$labels~., data = testSet)
  reviews.logreg.pred <- predict(reviews.glmnet,
                                          newx=newX,s="lambda.min",type="class") 
  print(table(reviews.logreg.pred,testSet$labels))
}
