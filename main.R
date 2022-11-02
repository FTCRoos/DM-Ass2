source("dataset.R")
source("score.R")
source("multinomialNaiveBayes.R")
source("regression.R")
source("tree.R")
source("randomForest.R")
source("mcnemar.R")

#import data
# Only unigrams
ds.unigram <- getDatasets(includeBigrams=FALSE)
train.unigram <- ds.unigram$train
test.unigram <- ds.unigram$test
# bigrams and unigrams
ds.both <- getDatasets(includeBigrams=TRUE)
train.both <- ds.both$train
test.both <- ds.both$test

# bayes predictions
bayes.uni.pred <- multinomailNaiveBayes(train.unigram, test.unigram, seed=1, FALSE)
bayes.both.pred <- multinomailNaiveBayes(train.both, test.both, seed=1, TRUE)

# logistic regression predictions 
log.uni.pred <- logisticRegression(train.unigram, test.unigram, seed=1)
log.both.pred <- logisticRegression(train.both, test.both, seed=1)

# tree prediction
tree.uni.pred <- classificationTree(train.unigrams, test.unigram, seed=1)
tree.both.pred <- classificationTree(train.both, test.both, seed=1)

# forest predictions
forest.uni.pred <- classificationRandomForest(train.unigram, test.unigram, seed=1)
forest.both.pred <- classificationRandomForest(train.both, test.both, seed=1)

#statistical tests
#q1 (generative vs discriminative)
mcnemar.function(bayes.uni.pred, log.uni.pred, test.unigram)
mcnemar.function(bayes.both.pred, log.both.pred, test.both)
#q2 (forest vs linear)
mcnemar.function(forest.uni.pred, bayes.uni.pred, test.unigram)
mcnemar.function(forest.uni.pred, log.uni.pred, test.unigram)
mcnemar.function(forest.both.pred, bayes.both.pred, test.both)
mcnemar.function(forest.both.pred, log.both.pred, test.both)
#q3 (uni vs both)
mcnemar.function(tree.uni.pred, tree.both.pred, test.both)
mcnemar.function(forest.uni.pred, forest.both.pred, test.both)
mcnemar.function(log.uni.pred, log.both.pred, test.both)
mcnemar.function(bayes.uni.pred, bayes.both.pred, test.both)
