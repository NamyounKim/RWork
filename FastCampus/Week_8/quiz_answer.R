##################################################################################################
# 1. sparse 값이 0.9인 dtm으로 naive bayes 모델 생성 후  모델의 민감도, 특이도, 정확도, AUC를 구하고
#    ROC커브를 기존 모델과 비교해보세요.
##################################################################################################

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm2 = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(dtm2) = trimws(colnames(dtm2))
dtm2 = dtm2[,nchar(colnames(dtm2)) > 1]

#Sparse Terms 삭제
dtm2 <- removeSparseTerms(dtm2, as.numeric(0.9))


#Covert to Dataframe
dtmDf2 = as.data.frame(as.matrix(dtm2))

#중복 Column 삭제
dtmDf2 = dtmDf2[,!duplicated(colnames(dtmDf2))]

#DtmDf에 정답표 붙이기
dtmDf2$target = target_val$spam_yn

#Traing Set, Test Set 만들기
trainingSet2 = dtmDf2[1:8000,] #Training 데이터 8,000개
testSet2 = dtmDf2[8001:nrow(dtmDf2),] #Test 데이터 2,612개

trainingSet2$target = as.factor(trainingSet2$target)
nbModel2 = e1071::naiveBayes(target ~ ., data = trainingSet2, fL=1)
nbPred2 =  predict(nbModel2, newdata = testSet2[,1:(ncol(testSet2)-1)], type="raw")
re_nbPred2 =  predict(nbModel2, newdata = testSet2[,1:(ncol(testSet2)-1)])

CrossTable(table(testSet2$target, re_nbPred2), prop.chisq=FALSE)

# ROC 값 구하기
nbPrediction2 = prediction(nbPred2[,2], testSet2$target)
nbPerfomance2 = performance(nbPrediction2, "tpr", "fpr")

# ROC 커브 그리기
plot(nbPerfomance2, col="red")
par(new=TRUE)
plot(nbPerfomance)
par(new=TRUE)
plot(svmPerfomance, col="blue")
abline(a=0, b=1, lwd=2, lty=2) # a:절편, b:기울기, lwd:선두께, lty:선타입

# AUC 값 구하기
svmAuc2 = performance(nbPrediction2, measure = "auc")
unlist(svmAuc2@y.values)


##################################################################################################
# 2. naive bayes 모델 4겹 교차검증(createFolds에서 k값을 4로)하여 
#    ROC커브로 비교하기
##################################################################################################

#ROC 커브를 그리기 위한 함수 만들기
getNbPerfomance <- function(trainingSet, testSet){
  library(ROCR)
  trainingSet$target = as.factor(trainingSet$target)
  nbModel = e1071::naiveBayes(target ~ ., data = trainingSet, fL=1)

  #Spam 문서 예측하기
  nbProbs = predict(nbModel, testSet[,1:(ncol(testSet)-1)], type="raw")
  
  # ROC 값 구하기
  nbPrediction = prediction(nbProbs[,2], testSet$target)
  nbPerformance = performance(nbPrediction, "tpr", "fpr")
  
  return(nbPerformance)
}

#### 교차 검증 ####
folds = createFolds(dtmDf$target, k=4)

testSet1 = dtmDf[folds$Fold1,]
testSet2 = dtmDf[folds$Fold2,]
testSet3 = dtmDf[folds$Fold3,]
testSet4 = dtmDf[folds$Fold4,]

#교차 검증 샘플링으로 trainSet 만들기
trainSet1 = dtmDf[-folds$Fold1,]
trainSet2 = dtmDf[-folds$Fold2,]
trainSet3 = dtmDf[-folds$Fold3,]
trainSet4 = dtmDf[-folds$Fold4,]

nbp1 = getNbPerfomance(trainSet1, testSet1)
nbp2 = getNbPerfomance(trainSet2, testSet2)
nbp3 = getNbPerfomance(trainSet3, testSet3)
nbp4 = getNbPerfomance(trainSet4, testSet4)


# ROC 커브 그리기
plot(nbp1, col="red")
par(new=TRUE)
plot(nbp2, col="blue")
par(new=TRUE)
plot(nbp3)
par(new=TRUE)
plot(nbp4, col="gold")
abline(a=0, b=1, lwd=2, lty=2) # a:절편, b:기울기, lwd:선두께, lty:선타입

##############################
## for문으로 처리하는 방법  ##
##############################
trainList = list(trainSet1, trainSet2, trainSet3, trainSet4)
testList = list(testSet1, testSet2, testSet3, testSet4)
nbp = list(NULL)

for(i in 1:4){
  nbp[i] = getNbPerfomance(as.data.frame(trainList[i]), as.data.frame(testList[i]))
}

plot(nbp[[1]], col="red")
par(new=TRUE)
plot(nbp[[2]], col="blue")
par(new=TRUE)
plot(nbp[[3]])
par(new=TRUE)
plot(nbp[[4]], col="gold")
abline(a=0, b=1, lwd=2, lty=2) # a:절편, b:기울기, lwd:선두께, lty:선타입