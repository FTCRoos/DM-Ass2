library(entropy)

multinomailNaiveBayes <- function(trainingSet, testSet, seed, includeBigrams){
  set.seed(seed)
  labels.train <- trainingSet$labels
  trainingSet <- within(trainingSet, rm(labels))
  labels.test <- testSet$labels
  testSet <- within(testSet, rm(labels))
  # feature selection (with mutual information, only for unigrams)
  trainingSet.mi <- apply(trainingSet, 2, function(x, y){
    mi.plugin(table(x,y)/length(y))
  }, labels.train)
  
  trainingSet.mi.order <- order(trainingSet.mi, decreasing = T)
  
  # feature selection (with mutual information)
  training.dtm.mi <- apply(trainingSet, 2, function(x, y){
    mi.plugin(table(x,y)/length(y))
  }, labels.train)
  
  training.dtm.mi.order <- order(training.dtm.mi,decreasing = T)
  
  # print
  # print(dim(trainingSet))
  # print(training.dtm.mi[training.dtm.mi.order[1:10]])
  
  # first model (with feature selection according to mutual information) (only unigrams)
  accuracies.unigrams.mi.models <- sapply(c(2:307), function(num.features){
    model.mi <- train.mnb(trainingSet[,trainingSet.mi.order[1:num.features]], labels.train) 
    
    predictions.mi <- predict.mnb(model.mi, testSet[,trainingSet.mi.order[1:num.features]])

    conf.mat <- table(predictions.mi, labels.test)
    
    return (sum(diag(conf.mat)) / 180)
  })
  
  accuracies.unigrams.mat <- cbind(accuracies.unigrams.mi.models, c(2:307))
  accuracies.unigrams.best.n <- accuracies.unigrams.mat[which.max(accuracies.unigrams.mat[,1]), 2]
  #print(accuracies.unigrams.mat)
  #print(accuracies.unigrams.best.n)
  plot(accuracies.unigrams.mat[,2], accuracies.unigrams.mat[,1], xlab = "n", ylab = "accuracy", type = "l")
  
  model.unigrams.mi <- train.mnb(trainingSet[,trainingSet.mi.order[1:accuracies.unigrams.best.n]], labels.train) 
  print(model.unigrams.mi)
  
  naive.bayes.predictions.unigrams.mi <- predict.mnb(model.unigrams.mi, testSet[,trainingSet.mi.order[1:accuracies.unigrams.best.n]])
  naive.bayes.predictions.unigrams.mi.table <- table(naive.bayes.predictions.unigrams.mi, labels.test)
  print(naive.bayes.predictions.unigrams.mi.table)
  getScore(naive.bayes.predictions.unigrams.mi.table)
  
  #second model (with feature selection according to mutual information) (both unigrams and bigrams)
  accuracies.mi.models <- sapply(c(2:307), function(num.features){
    model.mi <- train.mnb(trainingSet[,training.dtm.mi.order[1:num.features]], labels.train) 
    
    predictions.mi <- predict.mnb(model.mi, testSet[,training.dtm.mi.order[1:num.features]])
    
    conf.mat <- table (predictions.mi, labels.test)
    
    return (sum(diag(conf.mat)) / 180)
  })
  
  accuracies.mat <- cbind(accuracies.mi.models, c(2:307))
  accuracies.best <- accuracies.mat[which.max(accuracies.mat[,1]), 2]
  #print(accuracies.mat)
  #print(accuracies.best)
  
  plot(accuracies.mat[,2], accuracies.mat[,1], xlab = "n", ylab = "accuracy", type = "l")
  model.mi <- train.mnb(trainingSet[,training.dtm.mi.order[1:accuracies.best] ], labels.train) 
  #print(model.mi)
  
  naive.bayes.predictions.mi <- predict.mnb(model.mi , testSet[,training.dtm.mi.order[1:accuracies.best]])
  naive.bayes.predictions.mi.table <- table(naive.bayes.predictions.mi, labels.test)
  print(naive.bayes.predictions.mi.table)
  getScore(naive.bayes.predictions.mi.table)
  
  if (includeBigrams) {
    return(naive.bayes.predictions.mi)
  } else {
    return(naive.bayes.predictions.unigrams.mi)
  }
}

# Naive Bayes training function: labels = classes
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
