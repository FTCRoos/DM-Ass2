library(tm)

readfolds <- function(path, foldNums) {
  reviews <- c()
  for (num in foldNums){
    p <- paste(path, paste("/fold",num, sep = ""), sep = "")
    print(p)
    filelist = list.files(path = p, full.names = TRUE)
    for (f in filelist){
      text <- readLines(f, encoding = "UTF-8")
      reviews <- append(reviews, text)
    }
  }
  return (reviews)
}

preprocess <- function(corpus.dec, corpus.true, includeBigrams){
  
  reviews.dec <-VCorpus(VectorSource(corpus.dec))
  reviews.true<-VCorpus(VectorSource(corpus.true))
  review.all<-c(reviews.dec,reviews.true)
  #clean the data
  review.all <- tm_map(review.all, content_transformer(tolower))
  review.all <- tm_map(review.all, removeNumbers)
  review.all <- tm_map(review.all, removePunctuation)
  review.all <- tm_map(review.all, stripWhitespace)
  review.all <- tm_map(review.all, removeWords,stopwords("english"))
  
  return (review.all)
}

getDatasets <- function(includeBigrams){

  #TRAINING SET
  deceptive.train <- readfolds("op_spam_v1.4/negative_polarity/deceptive_from_MTurk", c(1,2,3,4))
  truthful.train <- readfolds("op_spam_v1.4/negative_polarity/truthful_from_Web", c(1,2,3,4))
  training.dtm <- preprocess(deceptive.train, truthful.train)
  
  #extraction of unigrams
  training.dtm.unigrams <- DocumentTermMatrix(training.dtm)
  training.dtm.unigrams <- removeSparseTerms(training.dtm.unigrams,0.95)
  training.dtm.unigrams <- as.matrix(training.dtm.unigrams)
  
  #extraction of bigrams
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  training.dtm.bigrams <- DocumentTermMatrix(training.dtm,control = list(tokenize = BigramTokenizer))
  training.dtm.bigrams <- removeSparseTerms(training.dtm.bigrams,0.95)
  training.dtm.bigrams <- as.matrix(training.dtm.bigrams)
  
  #construct training set
  n <- length(training.dtm) / 2
  labels <- c(rep(0,n),rep(1,n))
  if (includeBigrams) {
    training.dtm <- cbind(training.dtm.unigrams, training.dtm.bigrams)
    training.dtm <- cbind(training.dtm, labels)
  } else {
    training.dtm <- cbind(training.dtm.unigrams, labels)
  }
  
  # TEST SET
  deceptive.test <- readfolds("op_spam_v1.4/negative_polarity/deceptive_from_MTurk", c(5))
  truthful.test <- readfolds("op_spam_v1.4/negative_polarity/truthful_from_Web", c(5))
  test.dtm <- preprocess(deceptive.test, truthful.test)

  #unigrams
  test.dtm.unigrams <- DocumentTermMatrix(test.dtm,list(dictionary=dimnames(training.dtm.unigrams)[[2]]))
  test.dtm.unigrams <- as.matrix(test.dtm.unigrams)

  #bigrams
  test.dtm.bigrams<- DocumentTermMatrix(test.dtm,list(dictionary=dimnames(training.dtm.bigrams)[[2]]))
  test.dtm.bigrams <- as.matrix(test.dtm.bigrams)
  
  #construct test set
  n <- length(test.dtm) / 2
  labels <- c(rep(0,n),rep(1,n))
  if (includeBigrams) {
    test.dtm <- cbind(test.dtm.unigrams, test.dtm.bigrams)
    test.dtm <- cbind(test.dtm, labels)
  } else {
    test.dtm <- cbind(test.dtm.unigrams,labels)
  }
  
  # COMBINE TRAIN AND TEST SET
  dataset <- list()
  dataset$train <- data.frame(training.dtm)
  dataset$test <- data.frame(test.dtm)
  
  return(dataset)
}

#TESTING
ds <- getDatasets(TRUE)
ds$train[1,]
