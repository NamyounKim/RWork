install.packages("tm") #텍스트 마이닝을 위한 패키지
install.packages("slam")
install.packages("dplyr")
install.packages("readr") #파일을 읽어오기 위한 패키지

# 메모리 크기 할당
options(java.parameters = c("-Xmx2g", "-Dfile.encoding=UTF-8"))

library(tm)
library(slam)
library(dplyr)
library(readr)
library(NLP4kec)
library(stringi)

# 1. 원문 데이터 및 사전 불러오기 ----------------------------------------------------------------------------------------------------
textData = readRDS("./raw_data/petitions_content_2018.RDS")

#동의어 / 불용어 사전 불러오기
stopWordDic = read_csv("./dictionary/stopword_ko.csv")
synonymDic = read_csv("./dictionary/synonym.csv")

# 2. 형태소 분석 및 전처리------------------------------------------------------------------------------------------------------------
#형태소 분석기 실행하기
#명사, 동사, 형용사만 추출
parsedData = r_parser_r(textData$content, language = "ko", useEn = T, korDicPath = "./dictionary/user_dictionary.txt")

#명사만 추출
parsedData_noun = r_extract_noun(textData$content, language = "ko", useEn = T, korDicPath = "./dictionary/user_dictionary.txt")

# 동의어 처리
parsedData = synonym_processing(parsedVector = parsedData, synonymDic = synonymDic)

#Corpus에 doc_id를 추가하기 위한 데이터 프레임 만들기
parsedData_df = data.frame(doc_id = textData$doc_id
                           ,text = parsedData)

saveRDS(parsedData_df, file = "./raw_data/parsed_petition_data.RDS") # 나중 재사용을 위해 저장

#Corpus 생성
corp = VCorpus(DataframeSource(parsedData_df))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#숫자 삭제
corp = tm_map(corp, removeNumbers)

#특정 단어 삭제
corp = tm_map(corp, removeWords, stopWordDic$stopword)


saveRDS(corp, file = "./raw_data/corpus_petition.RDS") # 나중 재사용을 위해 저장


# 3. DTM 생성 및 Sparse Term 삭제 ----------------------------------------------------------------------------------------------------------
# Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

# Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm = TermDocumentMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))

# Sparse Terms 삭제 (값이 작아질 수록 term수가 줄어든다.)
dtm = removeSparseTerms(dtm, as.numeric(0.98))

# 단어 발생 빈도 구하기
freq = colSums(as.matrix(dtm))

# DTM을 데이터 프레임 형식으로 저장하기
dtm_df = as.data.frame(as.matrix(dtm))


# 4. 단어 빈도 정보 추출하기 ----------------------------------------------------------------------------------------------------------
#단어 개수 구하기
length(freq)

#내림차순으로 단어 10개, sorting 하기
freq[head(order(-freq), 10)]
sort(freq, decreasing = T)[1:10]

#오름차순으로 단어 10개 sorting 하기
freq[head(order(freq), 10)]
sort(freq, decreasing = F)[1:10]

#특정 빈도 사이값을 갖는 단어 구하기 (20보다 크고 341보다 작은 단어)
findFreqTerms(dtm, lowfreq = 20, highfreq = 341)

#단어 빈도 정보 Data Set 만들기
wordDf = data.frame(word=names(freq), freq=freq)


# 5. 단어 빈도 정보로 시각화 하기 ------------------------------------------------------------------------------------------------------
library(ggplot2)

#맥북 사용자는 폰트 import하기
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="postscript")

#상위 단어 10개만 바차트로 보여주기
top10 = wordDf %>% top_n(10) #상위 10개 단어만 추출
ggplot(top10, aes(x=word, y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family = "AppleGothic"))

#상위 10개 단어 빈도순으로 정렬하여 바차트로 보여주기
ggplot(top10, aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family = "AppleGothic"))


# Word Cloud 그리기
install.packages("wordcloud2")
library(wordcloud2)

top100 = wordDf %>% top_n(100) # 상위 100개 단어만 추출

wordcloud2(data = top100
           , color = "random-light"
           , shape = "star"
           , size = 0.5
           , fontFamily = "나눔고딕")

#treeMap 그리기
install.packages("treemap")
library(treemap)
treemap(top100 # 대상 데이터 설정
        ,title = "Word Tree Map"
        ,index = c("word") # 박스 안에 들어갈 변수 설정
        ,vSize = "freq"  # 박스 크기 기준
        ,fontfamily.labels = "AppleGothic" # 맥 폰트 설정
        ,fontsize.labels = 12 # 폰트 크기 설정
        #,palette=pal # 위에서 만든 팔레트 정보 입력
        ,border.col = "white") # 경계선 색깔 설정
