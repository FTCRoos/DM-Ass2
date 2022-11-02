mcnemar.function <- function (model1.pred, model2.pred, test) {
  # count correct model 1
  model1.pred.is.correct <- sapply(c(1:length(model1.pred)),function(index){
    if(model1.pred[index] == test$labels[index]){
      return (1)
    }
    else (return (0))
  })
  # count correct model 2
  model2.pred.is.correct <- sapply(c(1:length(model2.pred)),function(index){
    if(model2.pred[index] == test$labels[index]){
      return (1)
    }
    else (return (0))
  })
  
  conf.matrix <- table(model1.pred.is.correct, model2.pred.is.correct)
  print(mcnemar.test(conf.matrix))
}
