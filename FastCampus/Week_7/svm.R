#package check & install & load
library(dplyr)
library(stringi)
library(tm)
library(pROC)
library(slam)
library(gmodels)
library(e1071)
library(klaR)
library(readr)
library(NLP4kec)

#형태소 분석기 실행하기
parsedData = file_parser_r(path = "./raw_data/Blog_TrainingSet_Spam.xlsx"
                           ,language = "ko"
                           ,korDicPath = "./dictionary/user_dictionary.txt")

saveRDS(parsedData, "./raw_data/parsedData.RDS") #데이터셋 저장하기
parsedData = readRDS("./raw_data/parsedData.RDS") #저장한 데이터셋 불러오기

# 예측 변수값 가져오기
target_val = read_csv("./raw_data/training_target_val.csv")

#동의어 / 불용어 사전 불러오기
stopWordDic = read_csv("./dictionary/stopword_ko.csv")
synonymDic = read_csv("./dictionary/synonym.csv")

###################################################################
#---------------------- Text Pre-processing ----------------------#
###################################################################

# 동의어 처리
for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], parsedData))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    parsedData[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], parsedData[docNum])
  }
}

## 단어간 스페이스 하나 더 추가하기 ##
parsedData = gsub(" ","  ",parsedData)

#Corpus 생성
corp = VCorpus(VectorSource(parsedData))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#소문자로 변경
corp = tm_map(corp, tolower)

#특정 단어 삭제
corp = tm_map(corp, removeWords, stopWordDic$stopword)

#텍스트문서 형식으로 변환
corp = tm_map(corp, PlainTextDocument)

##################################################################

#텍스트문서 형식으로 변환
corp = tm_map(corp, PlainTextDocument)

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

#Sparse Terms 삭제
dtm <- removeSparseTerms(dtm, as.numeric(0.98))

#Covert to Dataframe
dtmDf = as.data.frame(as.matrix(dtm))

#중복 Column 삭제
dtmDf = dtmDf[,!duplicated(colnames(dtmDf))]

#DtmDf에 정답표 붙이기
dtmDf$target = target_val$spam_yn

#Traing Set, Test Set 만들기
trainingSet = dtmDf[1:8000,] #Training 데이터 8,000개
testSet = dtmDf[8001:nrow(dtmDf),] #Test 데이터 2,012개

dtmDf$target
tapply(dtmDf$target, dtmDf$target, length)

#########################
# SVM 모델링
#########################
trainingSet$target = as.factor(trainingSet$target)
svmModel = svm(target ~ . , data = trainingSet, type = "C-classification", 
               kernel="linear", gamma=0.1, cost=1)

svmModel = readRDS("./Week_7/svmModel.rds")

#Spam 문서 예측하기
svmPred =  predict(svmModel, newdata = testSet[,1:(ncol(testSet)-1)])

#Test 데이터 확인하기
tapply(testSet$target, testSet$target, length)

#예측 결과 확인하기
svm_pred_result = CrossTable(table(testSet$target, svmPred), prop.chisq=FALSE)

#정확도 계산하기
(svm_pred_result$t[1,1] + svm_pred_result$t[2,2]) / nrow(testSet)

########################################################################################################
# 정답지가 없는 새로운 문서를 분류할 경우            
########################################################################################################

# 새로운 문서 형태소 분석 실행하기
newData = file_parser_r(path = "./raw_data/Blog_TestSet_Spam.xlsx"
                        ,language = "ko"
                        ,korDicPath = "./dictionary/user_dictionary.txt")

#############################
#Text Pre-processing
#############################

# 동의어 처리
for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], newData))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    newData[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], newData[docNum])
  }
}

## 단어간 스페이스 하나 더 추가하기 ##
newData = gsub(" ","  ", newData)

#Corpus 생성
newCorp = VCorpus(VectorSource(newData))

#특수문자 제거
newCorp = tm_map(newCorp, removePunctuation)

#소문자로 변경
corp = tm_map(corp, tolower)

#특정 단어 삭제
newCorp = tm_map(newCorp, removeWords, stopWordDic$stopword)

#===============================================

#텍스트문서 형식으로 변환
newCorp = tm_map(newCorp, PlainTextDocument)

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
newDtm = DocumentTermMatrix(newCorp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(newDtm) = trimws(colnames(newDtm))
newDtm = newDtm[,nchar(colnames(newDtm)) > 1]

#Covert to Dataframe
newDtmDf = as.data.frame(as.matrix(newDtm))

#중복 Column 삭제
newDtmDf = newDtmDf[,!duplicated(colnames(newDtmDf))]

## Training Set Column과 newDtmDf 의 Column을 동일하게 만들어주는 처리
newDtmDf = newDtmDf[,colnames(newDtmDf) %in% colnames(trainingSet)] # 서로 같은 컬럼만 추출

add_col = trainingSet[,!colnames(trainingSet) %in% colnames(newDtmDf)] # trainingSet과 다른 컬럼 추출
add_column_nm = colnames(add_col) 

forAdd = data.frame(matrix(0, ncol=length(add_column_nm), nrow=nrow(newDtmDf))) # trainingSet과 다른 컬럼을 갖는 0으로 구성된 데이터 셋 만들기
colnames(forAdd)= add_column_nm # 컬럼명 부여하기

newDtmDf = cbind(newDtmDf, forAdd) # newDtmDf에 붙여주기

# 새로운 문서 Spam 문서 예측하기
svmPred_new = predict(svmModel, newDtmDf)

# 새로운 문서 예측결과 확인하기기
test_target_val = read_csv("./raw_data/test_target_val.csv")
svmPred_new_df = data.frame(pred = svmPred_new, original = test_target_val$spam_yn)
svmPred_new_result = CrossTable(table(svmPred_new_df$original, svmPred_new_df$pred), prop.chisq=FALSE)
(svmPred_new_result$t[1,1] + svmPred_new_result$t[2,2]) / length(newData)
