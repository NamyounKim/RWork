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


train <- trainDataProcessing(train)
test <- testDataProcessing(test)

#remove outlier
train<- subset(train, diffTemp <=5)

result <- NULL
result_registered <- NULL
rmse_sum <- NULL
rsquared_sum <- NULL

rsIndex <- 1
cvtrain <- trainControl(method="cv", number=5)
grid <- data.frame(mtry=c(5,6,7,8,9,10,11,12))

for(year_i in unique(train$year)){
  for(hour_i in unique(train$hourRange)){
    train_s <- train[(train$year == year_i) & (train$hourRange == hour_i),]
    test_s <- test[(test$year == year_i) & (test$hourRange == hour_i),]
    
    rf <- train(x=selectVariable(train_s), y=train_s$logCount, method="parRF",
                trControl=cvtrain,
                tuneGrid=grid,
                ntree=100,
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
    print(paste(year_i, " ", hour_i))
    print(rmse_sum[rsIndex])
    rsIndex <- rsIndex+1
  }
}

write.csv(subset(result, select = c(datetime,count)), file="./result.csv", row.names = FALSE)


#Validation
mean(rmse_sum)
mean(rsquared_sum)
rmseSumDf <- as.data.frame(rmse_sum)
colnames(rmseSumDf) <- c("rmse")
rmseSumDf$year[1:24]<-"2011"
rmseSumDf$year[25:48]<-"2012"
rmseSumDf$hours[1:24] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
rmseSumDf$hours[25:48] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
ggplot(rmseSumDf, aes(x=hours, y=rmse, fill=year)) + geom_bar(stat = "identity", position="dodge")


