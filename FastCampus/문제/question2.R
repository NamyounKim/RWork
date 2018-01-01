
###### 1. sparse 값이 0.98, gamma가 0.1, cost가 2인 SVM 모델 만들어서 기존 모델과 비교해보기
#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm2 = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(dtm2) = trimws(colnames(dtm2))
dtm2 = dtm2[,nchar(colnames(dtm2)) > 1]

#Sparse Terms 삭제
dtm2 <- removeSparseTerms(dtm2, as.numeric(0.98))


#Covert to Dataframe
dtmDf2 = as.data.frame(as.matrix(dtm2))

#중복 Column 삭제
dtmDf2 = dtmDf2[,!duplicated(colnames(dtmDf2))]

#DtmDf에 정답표 붙이기
dtmDf2$target = target_val$spam_yn

#Traing Set, Test Set 만들기
trainingSet2 = dtmDf2[1:8000,] #Training 데이터 8,000개
testSet2 = dtmDf2[8001:nrow(dtmDf2),] #Test 데이터 2,612개

trainingSet2$target = as.factor(trainingSet$target)
svmModel2 = svm(target ~ . , data = trainingSet2, type = "C-classification", kernel="linear", gamma=0.1, cost=2)
svmPred2 =  predict(svmModel2, newdata = testSet2[,1:(ncol(testSet2)-1)], decision.values = TRUE)

svmProbs2 = attr(svmPred2,"decision.values")

# ROC 값 구하기
svmPrediction2 = prediction(svmProbs2, testSet2$target)
svmPerfomance2 = performance(svmPrediction2, "tpr", "fpr")

# ROC 커브 그리기
plot(svmPerfomance2, col="red")
par(new=TRUE)
plot(svmPerfomance)
abline(a=0, b=1, lwd=2, lty=2) # a:절편, b:기울기, lwd:선두께, lty:선타입

# AUC 값 구하기
svmAuc2 = performance(svmPrediction2, measure = "auc")
unlist(svmAuc2@y.values)

# ROC 커브 비교하기
  

# 2. 4겹 교차검증하기 (createFolds에서 k값을 4로)