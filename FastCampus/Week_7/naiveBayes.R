#package check & install & load
install.packages(c("pROC","gmodels","klaR","e1071","readr"))
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
parsedData = text_parser(path = "./Blog_TrainingSet_Spam.xlsx"
                         ,language = "ko"
                         ,korDicPath = "./dictionary.txt")

saveRDS(parsedData, "./parsedData.RDS") #데이터셋 저장하기
parsedData = readRDS("./parsedData.RDS") #저장한 데이터셋 불러오기

# 예측 변수값 가져오기
target_val = read_csv("./training_target_val.csv")

## 단어간 스페이스 하나 더 추가하기 ##
parsedData = gsub(" ","  ",parsedData)

################################
#Text Pre-processing
################################
#Corpus 생성
corp = VCorpus(VectorSource(parsedData))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#소문자로 변경
corp = tm_map(corp, tolower)

#특정 단어 삭제
stopWord = read_csv("./stopword_ko.csv")
corp = tm_map(corp, removeWords, stopWord$stopword)

#동의어 처리
for (j in seq(corp))
{
  corp[[j]] <- gsub("lg", "엘지", corp[[j]])
  corp[[j]] <- gsub("sony", "소니", corp[[j]])
}

#텍스트문서 형식으로 변환
corp = tm_map(corp, PlainTextDocument)

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

#Sparse Terms 삭제
dtm_removed = removeSparseTerms(dtm, as.numeric(0.98))

#Covert to Dataframe
dtmDf = as.data.frame(as.matrix(dtm_removed))

#중복 Column 삭제
dtmDf = dtmDf[,!duplicated(colnames(dtmDf))]

#dtmDf$id = parsedData$id
dtmDf$target = target_val$spam_yn

#Traing Set, Test Set 만들기
trainingSet = dtmDf[1:8000,] #Training 데이터 8,000개
testSet = dtmDf[8001:nrow(dtmDf),] #Test 데이터 2,012개

#########################
# Naive Bayes 모델링
#########################
trainingSet$target = as.factor(trainingSet$target)
nbModel = e1071::naiveBayes(target ~ ., data = trainingSet, fL=1)

#Test 데이터 확인하기
tapply(testSet$target, testSet$target, length)

#Spam 문서 예측하기
nbPred = predict(nbModel, testSet[,1:(ncol(testSet)-1)])

#예측 결과 확인하기
nb_pred_result = CrossTable(table(testSet$target, nbPred), prop.chisq=FALSE)

#정확도 계산하기
(nb_pred_result$t[1,1] + nb_pred_result$t[2,2]) / nrow(testSet)

########################################################################################################
# 정답지가 없는 새로운 문서를 분류할 경우             
########################################################################################################

# 새로운 문서 형태소 분석 실행하기
newData = text_parser(path = "./Blog_TestSet_Spam.xlsx"
                         ,language = "ko"
                         ,korDicPath = "./dictionary.txt")

## 단어간 스페이스 하나 더 추가하기 ##
newData = gsub(" ","  ", newData)

#############################
#Text Pre-processing
#############################
#Corpus 생성
newCorp = VCorpus(VectorSource(newData))

#특수문자 제거
newCorp = tm_map(newCorp, removePunctuation)

#소문자로 변경
corp = tm_map(corp, tolower)

#특정 단어 삭제
newCorp = tm_map(newCorp, removeWords, c("있다", "하다","그렇다","되다","같다","가다","없다","보다","정도","000원","030원","주세요","어떻다"))

#동의어 처리
for (j in seq(newCorp))
{
  newCorp[[j]] <- gsub("lg", "엘지", newCorp[[j]])
  newCorp[[j]] <- gsub("sony", "소니", newCorp[[j]])
}

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
nbPred_new = predict(nbModel, newDtmDf)

# 새로운 문서 예측결과 확인하기
test_target_val = read_csv("./test_target_val.csv")
nbPred_new_df = data.frame(pred = nbPred_new, original = test_target_val$spam_yn)
nbPred_new_result = CrossTable(table(nbPred_new_df$original, nbPred_new_df$pred), prop.chisq=FALSE)
(nbPred_new_result$t[1,1] + nbPred_new_result$t[2,2]) / length(newData)

