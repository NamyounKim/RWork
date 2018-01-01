install.packages("tm") #텍스트 마이닝을 위한 패키지
install.packages("slam")
install.packages("dplyr")

library(tm)
library(slam)
library(dplyr)
library(NLP4kec)
library(readr)

#원문 데이터 가져오기
textData = readRDS("./raw_data/petitions.RDS")
textData = textData[nchar(textData$content)>30,] #불필요한 데이터 제거

#형태소 분석기 실행하기
parsedData = r_parser_r(textData$content, language = "ko", useEn = T, korDicPath = "./dictionary/user_dictionary.txt")

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
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

#Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))

#Sparse Terms 삭제 (값이 작아질 수록 term수가 줄어든다.)
dtm = removeSparseTerms(dtm, as.numeric(0.98))

#단어 발생 빈도 구하기
freq = colSums(as.matrix(dtm))

#DTM을 데이터 프레임 형식으로 저장하기
dtm_df = as.data.frame(as.matrix(dtm))

#DTM을 CSV로 추출해서 확인해보기
write_excel_csv(dtm_df, "./dtm.csv")

#단어 개수 구하기
length(freq)

#내림차순으로 단어 10개, sorting 하기
freq[head(order(-freq), 5)]

#오름차순으로 단어 10개 sorting 하기
freq[head(order(freq), 10)]

#특정 빈도 사이값을 갖는 단어 구하기 (20보다 크고 341보다 작은 단어)
findFreqTerms(dtm, lowfreq = 20, highfreq = 341)

#단어 빈도 시각화
wordDf = data.frame(word=names(freq), freq=freq)
library(ggplot2)

#단어 빈도수 바차트로 보여주기
ggplot(wordDf, aes(x=word, y=freq)) + geom_bar(stat = "identity")

#단어 10개만 바차트로 보여주기
ggplot(head(wordDf,10), aes(x=word, y=freq)) + geom_bar(stat = "identity")

#상위 20개 단어만 바차트로 보여주기
ggplot(head(arrange(wordDf,-freq),20), aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity")

# Word Cloud 그리기
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data = wordDf
           , color = "random-light"
           , shape = "star"
           , size = 0.5
           , fontFamily = "나눔고딕")

letterCloud(wordDf
            , word = "TEXT"
            , size = 1
            , fontFamily = "나눔고딕")

#treeMap 그리기
install.packages("treemap")
library(treemap)
treemap(wordDf # 대상 데이터 설정
        ,title = "Word Tree Map"
        ,index = c("word") # 박스 안에 들어갈 변수 설정
        ,vSize = "freq"  # 박스 크기 기준
        ,fontsize.labels = 12 # 폰트 크기 설정
        ,palette=pal # 위에서 만든 팔레트 정보 입력
        ,border.col = "white") # 경계선 색깔 설정
