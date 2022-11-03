library(entropy)

multinomailNaiveBayes <- function(trainingSet, testSet, seed){
  set.seed(seed)
  labels.train <- trainingSet$labels
  trainingSet <- within(trainingSet, rm(labels))
  labels.test <- testSet$labels
  testSet <- within(testSet, rm(labels))
  
  # feature selection with mutual information
  trainingSet.mi <- apply(trainingSet, 2, function(x, y){
    mi.plugin(table(x,y)/length(y))
  }, labels.train)
  
  trainingSet.mi.order <- order(trainingSet.mi, decreasing = T)
  
  print(trainingSet.mi.order)
  
  # print(dim(trainingSet))
  
  # training the model with feature selection according to mutual information
  accuracies.mi.models <- sapply(c(2:length(trainingSet)), function(num.features){
    print(num.features)
    model.mi <- train.mnb(trainingSet[,trainingSet.mi.order[1:num.features]], labels.train) 
    
    predictions.mi <- predict.mnb(model.mi, testSet[,trainingSet.mi.order[1:num.features]])

    conf.mat <- table(predictions.mi, labels.test)
    
    return (sum(diag(conf.mat)) / 180)
  })
  
  print(accuracies.mi.models)
  
  accuracies.mat <- cbind(accuracies.mi.models, c(2:length(testSet)))
  accuracies.best.n <- accuracies.mat[which.max(accuracies.mat[,1]), 2]
  
  print(trainingSet[,trainingSet.mi.order[1:accuracies.best.n]])
  
  plot(accuracies.mat[,2], accuracies.mat[,1], xlab = "n", ylab = "accuracy", type = "l")
  model.mi <- train.mnb(trainingSet[,trainingSet.mi.order[1:accuracies.best.n]], labels.train)
  
  # prints the list of probabilities
  print(model.mi)
  
  naive.bayes.predictions.mi <- predict.mnb(model.mi, testSet[,trainingSet.mi.order[1:accuracies.best.n]])
  naive.bayes.predictions.mi.table <- table(naive.bayes.predictions.mi, labels.test)
  
  getScore(naive.bayes.predictions.mi.table)
  
  return(naive.bayes.predictions.mi)
}

# Naive Bayes training function:
train.mnb <- function(dtm, labels) {
  call <- match.call()
  
  # vocabulary
  V <- ncol(dtm)
  
  # number of documents
  N <- nrow(dtm)
  
  prior <- table(labels) / N
  labelnames <- names(prior)
  nclass <- length(prior)
  cond.probs <- matrix(nrow=V, ncol=nclass)
  dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]]
  dimnames(cond.probs)[[2]] <- labelnames
  index <- list(length = nclass)
  
  for(j in 1:nclass){
    index[[j]] <- c(1:N)[labels == labelnames[j]]
  }
  
  for(i in 1:V){
    for(j in 1:nclass){
      # Laplace smoothing
      cond.probs[i,j] <- (sum(dtm[index[[j]],i]) + 1) / (sum(dtm[index[[j]],]) + V)
    }
  }
  
  x <- list(call=call, prior=prior, cond.probs=cond.probs)
  
  return (x)
}

# Naive Bayes predict function:
predict.mnb <- function(model, dtm) {
  classlabels <- dimnames(model$cond.probs)[[2]]
  
  logprobs <- as.matrix(dtm) %*% log(as.matrix(model$cond.probs))
  
  # number of documents to classify
  N <- nrow(dtm)
  
  # number of classes
  nclass <- ncol(model$cond.probs)
  logprobs <- logprobs + matrix(nrow = N, ncol = nclass, log(model$prior), byrow = T)
  x <- classlabels[max.col(logprobs)]  
  
  return (x)
}
