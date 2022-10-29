source("dataset.R")
source("score.R")
source("multinomialNaiveBayes.R")
source("regression.R")
source("tree.R")
source("randomForest.R")

#import data
# Only unigrams
ds.unigram <- getDatasets(includeBigrams=FALSE)
train.unigram <- ds.unigram$train
test.unigram <- ds.unigram$test
# bigrams and unigrams
ds.both <- getDatasets(includeBigrams=TRUE)
train.both <- ds.both$train
test.both <- ds.both$test

#
multinomailNaiveBayes(train.unigram, test.unigram, seed=1)

# Create 
#logisticRegression(train.unigram, test.unigram, seed=1)

# Construct and tune classification trees
#classificationTree(train.unigrams, test.unigrams, seed=1)
#classificationTree(train.both, test.both, seed=1)

#
#classificationRandomForest(train.unigram, test.unigram, seed=1)
