rm(list=ls())
gc()

#package check & install & load
libraryList <- c("dplyr","stringi","caret","reshape","randomForest","lubridate")

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

#Load Data
test <- read.csv(file="./2_bike/test.csv")
train <- read.csv(file="./2_bike/train.csv")
test[is.na(test),]
train[is.na(train),]

train$year <- year(ymd_hms(train$datetime))
train$month <- month(ymd_hms(train$datetime))
train$hour <- hour(ymd_hms(train$datetime))
train$weekdays <- weekdays(ymd_hms(train$datetime))
train$weekend <- ifelse((train$weekdays=="Sunday")|(train$weekdays=="Saturday"),1,0)
train$diffTemp <- train$temp - train$atemp
train$logCount <- log(train$count)
train$logCasual <- log(train$casual+1)
train$logRegistered <- log(train$registered+1)

test$year <- year(ymd_hms(test$datetime))
test$month <- month(ymd_hms(test$datetime))
test$hour <- hour(ymd_hms(test$datetime))
test$weekdays <- weekdays(ymd_hms(test$datetime))
test$weekend <- ifelse((test$weekdays=="Sunday")|(test$weekdays=="Saturday"),1,0)
test$diffTemp <- test$temp - test$atemp


selectVariable <- function(data) {
  selected <- c(
    "holiday",
    "workingday",
    "temp",
    "atemp",
    "humidity",
    "windspeed",
    "year",
    "hour"
  )
  return(data[,selected])
}

result <- NULL
rmse_sum <- NULL
rsIndex <- 1
cvtrain <- trainControl(method="cv", number=5)
grid <- data.frame(mtry=c(2,3,4,5,6,7,8))

for(weather_i in unique(train$weather)){
  for(season_i in unique(train$season)){
    train_s <- train[(train$weather == weather_i) & (train$season == season_i),]
    test_s <- test[(test$weather == weather_i) & (test$season == season_i),]
    
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
    print(paste(weather_i, " ", season_i))
    print(rf)
    rsIndex <- rsIndex+1
  }
}

write.csv(subset(result, select = c(datetime,count)), file="./result.csv", row.names = FALSE)


#Validation
mean(rmse_sum)
rmseSumDf <- as.data.frame(rmse_sum)
colnames(rmseSumDf) <- c("rmse")
rmseSumDf$year[1:24]<-"2011"
rmseSumDf$year[25:48]<-"2012"
rmseSumDf$hours[1:24] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
rmseSumDf$hours[25:48] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
ggplot(rmseSumDf, aes(x=hours, y=rmse, fill=year)) + geom_bar(stat = "identity", position="dodge")


