library(caret)
library(doMC)
registerDoMC(cores = 4)

cvtrain = trainControl(method="cv" # 샘플링 기법 선택
                       ,number=4 #교차검증 횟수 선택
                       ,classProbs = TRUE # 각 예측 범주(Class)에 대한 확률값
                       ,selectionFunction = "best" # 모델 선택 방법
                       ,summaryFunction = twoClassSummary # 2개의 범주를 가진 예측 모델에서 ROC, AUC값을 구할 경우 옵션
                       )

#########################
#----- Naive Bayes -----#
#########################
grid = data.frame(fL=c(1,2) # 비교할 라플라스 추정값을 입력한다.
                  ,usekernel=FALSE
                  ,adjust=FALSE)

nbModel_caret = train(target ~ ., data=dtmDf
                      ,method="nb"
                      ,trControl=cvtrain
                      ,tuneGrid= grid
                      ,metric= "ROC"
                      #,metric= "Accuracy"
                      ,preProc=c("center", "scale"))
saveRDS(nbModel_caret, "./Week_8/nbModel_caret.RDS")
nbModel_caret

#########################
#---------- SVM --------#
#########################
grid = data.frame(.C=c(1, 2)) # 비교할 Cost값을 입력한다.

svmModel_caret = train(target ~ ., data=dtmDf
                       ,method="svmLinear"
                       ,trControl=cvtrain
                       ,tuneGrid=grid
                       ,metric= "ROC"
                       ,preProc=c("center", "scale"))	

saveRDS(svmModel_caret, "./Week_8/svmModel_caret.RDS")

#########################
#---- Random Forest ----#
#########################
grid = data.frame(.mtry=c(32,33)) # 적정한 독립변수의 개수를 정한다. (전체 변수개수의 제곱근)

rfModel_caret = train(target ~ ., data=dtmDf
                      ,method="parRF"
                      ,trControl=cvtrain
                      ,ntree=100
                      ,tuneGrid=grid
                      ,metric="ROC"
                      ,preProc=c("center", "scale"))

saveRDS(rfModel_caret, "./Week_8/rfModel_caret.RDS")