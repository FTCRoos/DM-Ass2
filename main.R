#import data
# Only unigrams
ds.unigram <- getDatasets(includeBigrams=FALSE)
train.unigram <- ds.unigram$train
test.unigram <- ds.unigram$test
# bigrams and unigrams
ds.both <- getDatasets(includeBigrams=TRUE)
train.both <- ds.both$train
test.both <- ds.both$test

# Construct and tune classification trees
classificationTree(train.unigrams, test.unigrams, seed=1)
classificationTree(train.both, test.both, seed=1)
