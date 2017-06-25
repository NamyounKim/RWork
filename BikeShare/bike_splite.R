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

result_c <- NULL
result_r <- NULL
rmse_sum_c <- NULL
rmse_sum_r <- NULL
rsquared_sum_c <- NULL
rsquared_sum_r <- NULL

rsIndex <- 1
cvtrain <- trainControl(method="cv", number=5)
grid <- data.frame(mtry=c(4,5,6,7,8,9,10,11,12))

## Predict Casual
for(year_i in unique(train$year)){
  for(index_i in unique(train$season)){
    train_s <- train[(train$year == year_i) & (train$season == index_i),]
    test_s <- test[(test$year == year_i) & (test$season == index_i),]
    
    rf_c <- train(x=selectVariable_c(train_s), y=train_s$logCasual, method="parRF",
                  trControl=cvtrain,
                  tuneGrid=grid,
                  ntree=100,
                  metric="RMSE",
                  importance=TRUE
                  #preProc=c("center", "scale")
    )
    
    output <- predict(rf_c, selectVariable_c(test_s))
    
    #test_s$count <- output
    test_s$casual <- exp(output)
    result_c <- rbind(result_c, test_s)
    rmse_sum_c[rsIndex] <- rf_c$results[2][match(rf_c$bestTune$mtry, grid$mtry),]
    rsquared_sum_c[rsIndex] <- rf_c$results[3][match(rf_c$bestTune$mtry, grid$mtry),]
    print(paste(year_i, index_i))
    print(rmse_sum_c[rsIndex])
    rsIndex <- rsIndex+1
  }
}


rsIndex <- 1

## Predict Registered
for(year_j in unique(train$year)){
  for(index_j in unique(train$season)){
    train_s <- train[(train$year == year_j) & (train$season == index_j),]
    test_s <- test[(test$year == year_j) & (test$season == index_j),]
    
    rf_r <- train(x=selectVariable_r(train_s), y=train_s$logRegistered, method="parRF",
                  trControl=cvtrain,
                  tuneGrid=grid,
                  ntree=100,
                  metric="RMSE",
                  importance=TRUE
                  #preProc=c("center", "scale")
    )
    
    output <- predict(rf_r, selectVariable_r(test_s))
    
    #test_s$count <- output
    test_s$registered <- exp(output)
    result_r <- rbind(result_r, test_s)
    rmse_sum_r[rsIndex] <- rf_r$results[2][match(rf_r$bestTune$mtry, grid$mtry),]
    rsquared_sum_r[rsIndex] <- rf_r$results[3][match(rf_r$bestTune$mtry, grid$mtry),]
    print(paste("Registered  ",year_j, index_j))
    print(rmse_sum_r[rsIndex])
    rsIndex <- rsIndex+1
  }
}


result <- data.frame(datetime=result_r$datetime, count=(result_c$casual + result_r$registered))

write.csv(subset(result, select = c(datetime,count)), file="./result.csv", row.names = FALSE, fileEncoding="UTF-8")
mean(rmse_sum_c)
mean(rmse_sum_r)
mean(rsquared_sum_c)
mean(rsquared_sum_r)

#Validation

rmseSumDf <- as.data.frame(rmse_sum)
colnames(rmseSumDf) <- c("rmse")
rmseSumDf$year[1:24]<-"2011"
rmseSumDf$year[25:48]<-"2012"
rmseSumDf$hours[1:24] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
rmseSumDf$hours[25:48] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
ggplot(rmseSumDf, aes(x=hours, y=rmse, fill=year)) + geom_bar(stat = "identity", position="dodge")


