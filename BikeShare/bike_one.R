rm(list=ls())
gc()

#package check & install & load
libraryList <- c("dplyr","stringi","caret","reshape","randomForest","lubridate","ggplot2","psych","doParallel")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
require(dplyr)
require(stringi)
require(caret)
require(reshape)
require(lubridate)
require(randomForest)
require(e1071)
require(ggplot2)
require(psych)
require(doParallel)

source(file = "./function.R")
registerDoParallel()

#Load Data
train <- read.csv(file="./2_bike/train.csv")
test <- read.csv(file="./2_bike/test.csv")

#Data preprocessing
train <- trainDataProcessing(train)
test <- testDataProcessing(test)

#remove outlier
train<- subset(train, diffTemp <=5)

result <- NULL
rmse_sum <- NULL
rsquared_sum <- NULL

rsIndex <- 1
cvtrain <- trainControl(method="cv", number=5)
grid <- data.frame(mtry=c(3,4,5,6,7,8,9,10,11))

for(index_i in unique(train$hourRange)){
    train_s <- train[(train$hourRange == index_i),]
    test_s <- test[(test$hourRange == index_i),]
    
    rf <- train(x=selectVariable(train_s), y=train_s$logCount, method="parRF",
                trControl=cvtrain,
                tuneGrid=grid,
                ntree=200,
                metric="RMSE",
                importance=TRUE
                #preProc=c("center", "scale")
    )
    
    output <- predict(rf, selectVariable(test_s))
    
    #test_s$count <- output
    test_s$count <- exp(output)
    result <- rbind(result, test_s)
    rmse_sum[rsIndex] <- rf$results[2][match(rf$bestTune$mtry, grid$mtry),]
    rsquared_sum[rsIndex] <- rf$results[3][match(rf$bestTune$mtry, grid$mtry),]
    print(paste(index_i))
    print(varImp(rf))
    print(rmse_sum[rsIndex])
    rsIndex <- rsIndex+1
}

write.csv(subset(result, select = c(datetime,count)), file="./predicted_test.csv", row.names = FALSE, fileEncoding="UTF-8")



