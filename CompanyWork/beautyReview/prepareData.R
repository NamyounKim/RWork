dyn.load("/usr/java/jdk1.8.0_151/jre/lib/amd64/server/libjvm.so")
library(NLP4kec)
library(rJava)
library(readr)
library(data.table)
library(tm)
library(dplyr)
library(readxl)
library(stringi)

jul = file_parser_r(path = "./data/7월리뷰정리.xlsx"
                   ,useEn = T
                   ,language = "ko"
                   ,korDicPath = "../dic/dictionary.txt")

aug = file_parser_r(path = "./data/8월리뷰정리.xlsx"
                   ,useEn = T
                   ,language = "ko"
                   ,korDicPath = "../dic/dictionary.txt")

sep = file_parser_r(path = "./data/9월리뷰정리.xlsx"
                   ,useEn = T
                   ,language = "ko"
                   ,korDicPath = "../dic/dictionary.txt")

oct = file_parser_r(path = "./data/10월리뷰정리.xlsx"
                   ,useEn = T
                   ,language = "ko"
                   ,korDicPath = "../dic/dictionary.txt")


julXl = readxl::read_xlsx(path = "./data/7월리뷰정리.xlsx", sheet = 1)
augXl = readxl::read_xlsx(path = "./data/8월리뷰정리.xlsx", sheet = 1)
sepXl = readxl::read_xlsx(path = "./data/9월리뷰정리.xlsx", sheet = 1)
octXl = readxl::read_xlsx(path = "./data/10월리뷰정리.xlsx", sheet = 1)

parsedText = c(jul,aug,sep,oct)
rawData = rbind(julXl,augXl)
rawData = rbind(rawData,sepXl)
rawData = rbind(rawData,octXl)
rm(julXl,augXl,sepXl,octXl)

stopWordDic = read_csv("../dic/stopword_ko.csv")
synonymDic = read_csv("../dic/synonym")


# 동의어 처리
for (i in 1:nrow(synonymDic)){
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], parsedText))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    parsedText[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], parsedText[docNum])
  }
}

# 브랜드명 수정
brandName = read_delim("./brand명수정", delim = ",", col_names = F)
colnames(brandName) = c("originWord","changeWord")
rawData$brand = trimws(rawData$brand)

for (i in 1:nrow(rawData)){
  targetBrandIdx = which(rawData$brand[i] == as.vector(brandName$originWord))
  
  if(length(targetBrandIdx) > 0){
    rawData$brand[i] = stri_replace_all_fixed(rawData$brand[i], brandName$originWord[targetBrandIdx], brandName$changeWord[targetBrandIdx])
    print(paste(i,"::",brandName$originWord[targetBrandIdx],"   ",rawData$brand[i]))
  }
}

rawData$parsed = parsedText

##################################################################
#Text Pre-processing
##################################################################
makeDtm <- function(parsedText, sr, dtmType){
  
  parsedText = as.vector(parsedText$parsed)
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
  #Document Term Matrix 생성 (단어 Length는 2로 세팅)
  if(dtmType == "tf"){
    dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
    dtm = removeSparseTerms(dtm, as.numeric(sr))
    
    return(dtm)
  }else if(dtmType == "tfidf"){
    dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                                 weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기
    dtmW = removeSparseTerms(dtmW, as.numeric(sr))
    return(dtmW)
  }else if(dtmType == "tdm-tf"){
    tdm = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf))) #Tf-Idf 가중치 주기))
    tdm = removeSparseTerms(tdm, as.numeric(sr))
  }else if(dtmType == "tdm-tfidf"){
    tdm = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf),
                                                weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기))
    tdm = removeSparseTerms(tdm, as.numeric(sr))
  }
}




