install.packages("caret")
library(caret)

#### 층화 홀드아웃 샘플링 ####
# trainSet 대상 row index 가져오기
inTrain = createDataPartition(dtmDf$target, p=0.75, list = FALSE)

#추출한 row index로 trainSet 만들기
trainSet = dtmDf[inTrain,]

#비율 확인하기
tapply(dtmDf$target, dtmDf$target, function(y) length(y)/nrow(dtmDf))
tapply(trainSet$target, trainSet$target, function(y) length(y)/nrow(trainSet))


#### 교차 검증 ####
folds = createFolds(dtmDf$target, k=5)

#추출한 row index로 trainSet 만들기
trainSet1 = dtmDf[folds$Fold1,]
trainSet2 = dtmDf[folds$Fold2,]
trainSet3 = dtmDf[folds$Fold3,]
trainSet4 = dtmDf[folds$Fold4,]
trainSet5 = dtmDf[folds$Fold5,]

#비율 확인하기
tapply(trainSet1$target, trainSet1$target, function(y) length(y)/nrow(trainSet1))
tapply(trainSet4$target, trainSet4$target, function(y) length(y)/nrow(trainSet4))

#### 부트스트랩 샘플링 ####
bootStrap = createResample(dtmDf$target, times = 5)

#추출한 row index로 trainSet 만들기
trainSet1 = dtmDf[bootStrap$Resample1,]
trainSet2 = dtmDf[bootStrap$Resample2,]
trainSet3 = dtmDf[bootStrap$Resample3,]
trainSet4 = dtmDf[bootStrap$Resample4,]
trainSet5 = dtmDf[bootStrap$Resample5,]

#비율 확인하기
tapply(trainSet1$target, trainSet1$target, function(y) length(y)/nrow(trainSet1))
tapply(trainSet4$target, trainSet4$target, function(y) length(y)/nrow(trainSet4))
