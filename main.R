library(rpart)
library(rpart.plot)

print("main.R")

#import data
ds <- getDatasets(includeBigrams=FALSE)
train <- ds$train
test <- ds$test

#TEST
rpart.unigrams <- rpart(labels~., data=train,cp=0,method="class", minbucket = 1, minsplit = 2 )