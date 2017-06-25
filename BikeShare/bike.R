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

train$year <- as.factor(year(ymd_hms(train$datetime)))
train$month <- as.factor(month(ymd_hms(train$datetime)))
train$hour <- as.factor(hour(ymd_hms(train$datetime)))
train$weekdays <- as.factor(weekdays(ymd_hms(train$datetime)))
train$dayType <- as.factor(ifelse((train$workingday==0)&(train$holiday==0),1,
                                  ifelse((train$workingday==0)&(train$holiday==1),2,
                                         ifelse((train$workingday==1)&(train$holiday==0),3,4))))
train$diffTemp <- train$temp - train$atemp
train$logCount <- log(train$count)
train$logCasual <- log(train$casual+1)
train$logRegistered <- log(train$registered+1)

train<- subset(train, diffTemp <=5)

test$year <- as.factor(year(ymd_hms(test$datetime)))
test$month <- as.factor(month(ymd_hms(test$datetime)))
test$hour <- as.factor(hour(ymd_hms(test$datetime)))
test$weekdays <- as.factor(weekdays(ymd_hms(test$datetime)))
test$dayType <- as.factor(ifelse((test$workingday==0)&(test$holiday==0),1,
                                 ifelse((test$workingday==0)&(test$holiday==1),2,
                                        ifelse((test$workingday==1)&(test$holiday==0),3,4))))
test$diffTemp <- test$temp - test$atemp

selectVariable <- function(data) {
  selected <- c(
#               "season",
                "holiday",
                "workingday",
                "weekend",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed"
#                "year",
#                "hour"
                )
  #data$year <- year(ymd_hms(data$datetime))
  #data$hour <- hour(ymd_hms(data$datetime))
  
  return(data[,selected])
}

holidayCheck <- function(data){
  if(sum(data$holiday)==0){
    return(subset(data, select=c(-holiday)))
  }
  else{
    return(data)
  }
}

cvtrain <- trainControl(method="cv", number=5)

result <- NULL
rmse_sum <- 0
for(year_i in unique(train$year)){
  for(month_i in unique(train$month)){
    train_s <- train[(train$year == year_i) & (train$month == month_i),]
    test_s <- test[(test$year == year_i) & (test$month == month_i),]
    
    rf <- train(x=holidayCheck(selectVariable(train_s)), y=train_s$logCount, method="parRF",
                trControl=cvtrain,
                tuneGrid=data.frame(mtry=c(5,6,7)),
                ntree=100,
                metric="RMSE",
                importance=TRUE,
                preProc=c("center", "scale"))
    if(!is.null(grep("holiday",colnames(holidayCheck(selectVariable(train_s)))))){
      output <- predict(rf, selectVariable(test_s))
    }
    else{
      output <- predict(rf, subset(selectVariable(test_s), select = c(-holiday)))
    }
    #test_s$count <- output
    test_s$count <- exp(output)
    result <- rbind(result, test_s)
    rmse_sum <- rmse_sum + rf$results[2]
    print(paste(year_i, " ", month_i))
    print(rf)
  }
}
rmse_sum/48

write.csv(subset(result, select = c(datetime,count)), file="./result.csv", row.names = FALSE)




#predict
pred <- predict(rf,test_rf)
test$pred <- pred
result <- subset(test, select = c(datetime,pred))
colnames(result)[2] <- "count"
write.csv(result, file="./result.csv", row.names = FALSE)
