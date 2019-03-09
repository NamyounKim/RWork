library(pROC)
library(dplyr)
library(stringi)
library(tm)
library(pROC)
library(slam)
library(gmodels)
library(e1071)
library(klaR)
library(readr)
install.packages("pROC")

saveRDS(svmModel, "./파일경로/파일명.rds")
svmModel = readRDS("./Week_7/svm_model_181017.RDS")

testSet = readRDS("./testSet_copy.RDS")

## SVM 모델로 TestSet 예측하기. 이때 decision.values 옵셥을 주어 예측 결정값을 계산하다.
svmPred =  predict(svmModel, newdata = testSet[,1:(ncol(testSet)-1)]
                   ,decision.values = T)

#### 오분류표 해석하기 ####
#오분류표 확인하기
svm_pred_result = CrossTable(table(testSet$target, svmPred), prop.chisq=FALSE)

#정확도(Accuracy) 계산하기
(svm_pred_result$t[1,1] + svm_pred_result$t[2,2]) / nrow(testSet)

#오차비율(Error Rate) 계산하기
(svm_pred_result$t[1,2] + svm_pred_result$t[2,1]) / nrow(testSet)

#민감도(Sensitivity) or 재현율(Recall) 계산하기
svm_pred_result$t[2,2] / (svm_pred_result$t[2,2] + svm_pred_result$t[2,1])

#특이도(Specificity) 계산하기
svm_pred_result$t[1,1] / (svm_pred_result$t[1,1] + svm_pred_result$t[1,2])

#정밀도(Precision) 계산하기 
svm_pred_result$t[2,2] / (svm_pred_result$t[2,2] + svm_pred_result$t[1,2])


#### SVM 예측 모델의 ROC 커브 그려보기 ####
install.packages("ROCR")
library(ROCR)

# 예측  결정값 가져오기
svmProbs = attr(svmPred,"decision.values")

# ROC 값 구하기
svmPrediction = prediction(svmProbs, testSet$target) # Spam 예측 확률값과 실제 분류값을 파라미터로 사용
svmPerfomance = performance(svmPrediction, "tpr", "fpr")

# ROC 커브 그리기
plot(svmPerfomance)
abline(a=0, b=1, lwd=2, lty=2) # a:절편, b:기울기, lwd:선두께, lty:선타입

# AUC 값 구하기
svmAuc = performance(svmPrediction, measure = "auc")
unlist(svmAuc@y.values)

# Cut off 그래프 그리기
plot(performance(svmPrediction, measure = "acc"))


##### Naive Bayes 모델 ROC커브로 비교해보기 #####
trainingSet = readRDS("./trainingSet.RDS")
nbModel = e1071::naiveBayes(target ~ ., data = trainingSet, fL=1)

#Spam 문서 예측 확률값 가져오기
nbProbs =  predict(nbModel, newdata = testSet2[,1:(ncol(testSet2)-1)]
                   ,type="raw")

# ROC 값 구하기
nbPrediction = prediction(nbProbs[,2], testSet$target)
nbPerfomance = performance(nbPrediction, "tpr", "fpr")

# ROC 커브 그리기
par(new=TRUE)
plot(nbPerfomance)
abline(a=0, b=1, lwd=2, lty=2) # a:절편, b:기울기, lwd:선두께, lty:선타입


# AUC 값 구하기
nbAuc = performance(nbPrediction, measure = "auc")
unlist(nbAuc@y.values)

# Cut off 그래프 그리기
plot(performance(nbPrediction, measure = "acc"))


##### Random Forest 모델 ROC커브로 비교해보기 #####
#Spam 문서 예측 확률값 가져오기
rfModel_caret = readRDS("./Week_7/rfModel_caret_181017.RDS")
rfPred =  predict(rfModel_caret, newdata = testSet[,1:(ncol(testSet)-1)]
                  ,type = "prob")

# ROC 값 구하기
rfPrediction = prediction(rfPred[,2], testSet$target)
rfPerfomance = performance(rfPrediction, "tpr", "fpr")

# ROC 커브 그리기
par(new=TRUE)
plot(rfPerfomance, col="blue")

# AUC 값 구하기
rfAuc = performance(rfPrediction, measure = "auc")
unlist(rfAuc@y.values)

# Cut off 그래프 그리기
plot(performance(rfPrediction, measure = "acc"))

