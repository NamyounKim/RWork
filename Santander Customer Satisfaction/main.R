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

train <- read.csv("./train.csv")
#train_s <- subset(train, select = c(ID,TARGET))
train_s <- train[,!(colnames(train) %in% c("ID","TARGET"))]

ggplot(train, aes(x=factor(TARGET))) + geom_bar()

ggplot(train, aes(x=factor(TARGET), y=saldo_var30)) + geom_boxplot()
ggplot(train[(train$var3>0 & train$var3<50),], aes(x=factor(TARGET), y=var3)) + geom_boxplot()

cvtrain <- trainControl(method="cv", number=5)
grid <- data.frame(mtry=c(19,40,80,100,200))

rf <- train(x=train_s, y=train$TARGET, method="parRF",
            trControl=cvtrain,
            tuneGrid=grid,
            ntree=200,
            metric="Accuracy",
            importance=TRUE
            #preProc=c("center", "scale")
)


