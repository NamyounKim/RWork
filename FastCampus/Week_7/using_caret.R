library(caret)

#### Naive Bayes ####
cvtrain <- trainControl(method="cv", number=3, classProbs = TRUE, selectionFunction = "best")
grid <- data.frame(fL=c(1,2), usekernel=FALSE, adjust=FALSE)

nbModel_caret <- train(target ~ ., data=dtmDf, method="nb",	
                        trControl=cvtrain,	
                        tuneGrid=grid,
                        metric="Kappa",
                        preProc=c("center", "scale"))

#### SVM ####
cvtrain <- trainControl(method="cv", number=3, classProbs = TRUE, selectionFunction = "best")
grid <- data.frame(.C=c(1, 2))

svmModel_caret <- train(target ~ ., data=dtmDf, method="svmLinear",	
                        trControl=cvtrain,	
                        tuneGrid=grid,
                        metric="Accuracy",
                        preProc=c("center", "scale"))	

saveRDS(svmModel_caret, "./svmModel_caret.rds")
svmModel_caret = readRDS("./svmModel_caret.rds")
