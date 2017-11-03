Sys.setenv(JAVA_HOME="C:/Program Files/JAVA/jre1.8.0_131/")
library(NLP4kec)
library(rJava)
library(readr)
library(tm)
library(data.table)

parsedText = text_parser("./targetSetForParser.csv", language = "ko", korDicPath = "./dictionary.txt")

stopWordDic = read_csv("./stopword_ko.csv")
synonymDic = read_csv("./synonym")

## 단어간 스페이스 하나 더 추가하기 ##
parsedText = gsub(" ","  ",parsedText)

# 동의어 처리
for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], parsedText))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    parsedText[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], parsedText[docNum])
  }
}


##################################################################
#Text Pre-processing
##################################################################

#Corpus 생성
corp = VCorpus(VectorSource(parsedText))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#숫자 삭제
corp = tm_map(corp, removeNumbers)

#소문자로 변경
corp = tm_map(corp, tolower)

#특정 단어 삭제
corp = tm_map(corp, removeWords, stopWordDic$stopword)


##################################################################

#텍스트문서 형식으로 변환
corp = tm_map(corp, PlainTextDocument)
saveRDS(corp, "./corp_1026.RDS")

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

saveRDS(dtm, "./dtm_1024.RDS")
dtm = readRDS("./dtm_1024.RDS")

#Sparse Terms 삭제 (값이 작아질 수록 term수가 줄어든다.)
dtm2 = removeSparseTerms(dtm, as.numeric(0.9999))
dtmDt = as.data.table(as.matrix(dtm2))

dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                             weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtmW) = trimws(colnames(dtmW))
dtmW = dtmW[,nchar(colnames(dtmW)) > 1]
saveRDS(dtmW, "./dtmW_1024.RDS")

dtmW = removeSparseTerms(dtmW, as.numeric(0.997))
dtmWDt = as.data.table(as.matrix(dtmW))
