
library(caret)
library(randomForest)

testBulk <- read.csv("pml-testing.csv", na.strings = c("NA", ""))
trainBulk <- read.csv("pml-training.csv", na.strings = c("NA", ""))
NAs <- apply(trainBulk, 2, function(x) {
    sum(is.na(x))
})
cleanTrain <- trainBulk[, which(NAs == 0)]
cleanTest <- testBulk[, which(NAs == 0)]

trainIndex <- createDataPartition(y = cleanTrain$classe, p = 0.7, list = FALSE)
trainSet <- cleanTrain[trainIndex, ]
crossValidationSet <- cleanTrain[-trainIndex, ]
removeIndex <- as.integer(c(1, 2, 3, 4, 5, 6))
trainSet <- trainSet[, -removeIndex]
testSet <- cleanTest[, -removeIndex]

mytrControl = trainControl(method = "cv", number = 4)
modelFit <- train(trainSet$classe ~ ., data = trainSet, method = "rf", trControl = mytrControl)

modelFit

predicted <- predict(modelFit, crossValidationSet)
SampleError <- sum(predicted == crossValidationSet$classe)/nrow(crossValidationSet)

answers <- predict(modelFit, testSet)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)


