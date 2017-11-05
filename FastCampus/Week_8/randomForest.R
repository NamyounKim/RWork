library(dplyr)
library(stringi)
library(tm)
library(pROC)
library(slam)
library(gmodels)
library(e1071)
library(klaR)
library(readr)
library(caret)
library(NLP4kec)

#형태소 분석기 실행하기
negSet = text_parser(path = "./Week_8/comment_neg.xlsx"
                         ,language = "ko"
                         ,korDicPath = "./dictionary.txt")

posSet = text_parser(path = "./Week_8/comment_pos.xlsx"
                     ,language = "ko"
                     ,korDicPath = "./dictionary.txt")

# NULL 값 처리하기 (댓글은 원문이 짧기 때문에 형태소 분석 결과가 없는 경우가 발생한다.)
negSet = negSet[negSet != ""]
posSet = posSet[posSet != ""]

# Data Frame 형식으로 바꾸기
negSet = data.frame(negSet, stringsAsFactors = F)
posSet = data.frame(posSet, stringsAsFactors = F)

# Target 변수 추가하기
negSet$sentiment = "neg"
posSet$sentiment = "pos"

# Pos Set의 데이터 크기가 크기 때문에 문자열이 긴 데이터 순으로 1645개 추출
posSet$len = nchar(posSet$posSet)
posSet = posSet %>% arrange(-len)
head_posSet = head(posSet, 1645)

#컬럼명 변경하기
colnames(negSet) = c("parsedContent","sentiment")
colnames(head_posSet) = c("parsedContent","sentiment")

# 부정, 긍정 문서 합치기
bindData = rbind(negSet, head_posSet[,1:2])

## 단어간 스페이스 하나 더 추가하기 ##
bindData$parsedContent = gsub(" ","  ",bindData$parsedContent)

##################################################################
#Text Pre-processing
##################################################################
#Corpus 생성
corp = VCorpus(VectorSource(bindData$parsedContent))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#Stopword 사전 읽어오기
stopWord = read_csv("./stopword_ko.csv")

#Stopword 삭제
corp = tm_map(corp, removeWords, stopWord$stopword)

#동의어 처리
for (j in seq(corp))
{
  corp[[j]] <- gsub("미장센", "미쟝센", corp[[j]])
  corp[[j]] <- gsub("미쟝셴", "미쟝센", corp[[j]])
  corp[[j]] <- gsub("미쟝셴", "미쟝센", corp[[j]])
}
##################################################################

#텍스트문서 형식으로 변환
corp = tm_map(corp, PlainTextDocument)

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

#Sparse Terms 삭제
dtm = removeSparseTerms(dtm, as.numeric(0.98))

#Covert to Dataframe
dtmDf = as.data.frame(as.matrix(dtm))

#중복 Column 삭제
dtmDf = dtmDf[,!duplicated(colnames(dtmDf))]

#DtmDf에 정답표 붙이기
dtmDf$target = bindData$sentiment

#### Random Forest ####
dtmDf$target = as.factor(dtmDf$target)

cvtrain = trainControl(method="cv"
                       , number=3
                       , classProbs = TRUE
                       , summaryFunction = twoClassSummary)
#cvtrain = trainControl(method="cv", number=7, classProbs = TRUE)

grid = data.frame(mtry=c(8,9)) # 적정한 독립변수의 개수를 정한다. (전체 변수개수의 제곱근)

rfModel_caret = train(target ~ ., data=dtmDf, method="parRF",	
                        trControl=cvtrain,	
                        tuneGrid=grid,
                        metric="Accuracy",
                        preProc=c("center", "scale"))	

# 중요변수 확인하기
importance_var = varImp(rfModel_caret)
plot(importance_var)

# Data Frame 형식으로 정보추출
imp_df = importance_var$importance
