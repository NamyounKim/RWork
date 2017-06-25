system("tctStart")
#system("/Users/kimnamyoun/TextConvert4TM/Start.sh")

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

# 형태소분석 완료된 문서 가져오기
negSet =read_csv("c:/TextConvert4TM_v1.0/output/out_comment_neg.csv") # 부정문서 형태소 분석 결과
posSet =read_csv("c:/TextConvert4TM_v1.0/output/out_comment_pos.csv") # 긍정문서 형태소 분석 결과

# Target 변수 추가하기
negSet$sentiment = "neg"
posSet$sentiment = "pos"

# NULL 값 처리하기 (댓글은 원문이 짧기 때문에 형태소 분석 결과가 없는 경우가 발생한다.)
negSet = negSet[!is.na(negSet$parsedContent),]
posSet = posSet[!is.na(posSet$parsedContent),]

# Pos Set의 데이터 크기가 크기 때문에 문자열이 긴 데이터 순으로 1645개 추출
posSet$len = nchar(posSet$parsedContent)
posSet = posSet %>% arrange(-len)
posSet2 = head(posSet, 1645)

# 부정, 긍정 문서 합치기
bindData = rbind(negSet, posSet2[,1:3])

#컬럼명 변경하기
colnames(bindData) = c("id","pContent","sentiment")

## 단어간 스페이스 하나 더 추가하기 ##
parsedDataRe = bindData
parsedDataRe$pContent = gsub(" ","  ",parsedDataRe$pContent)

##################################################################
#Text Pre-processing
##################################################################
#Corpus 생성
corp = VCorpus(VectorSource(parsedDataRe$pContent))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#특정 단어 삭제
corp = tm_map(corp, removeWords, c("있다", "하다","그렇다","되다","같다","가다","없다","보다","정도","000원","030원","주세요","어떻다"))

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
dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

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
cvtrain = trainControl(method="cv", number=3, classProbs = TRUE, summaryFunction = twoClassSummary)
#cvtrain = trainControl(method="cv", number=7, classProbs = TRUE)
grid = data.frame(mtry=9)
rfModel_caret = train(target ~ ., data=dtmDf, method="parRF",	
                        trControl=cvtrain,	
                        tuneGrid=grid,
                        metric="Accuracy",
                        preProc=c("center", "scale"))	

# 중요변수 확인하기
importance_var = varImp(rfModel_caret)
plot(importance_var)
