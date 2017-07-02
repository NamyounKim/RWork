install.packages("tm") #텍스트 마이닝을 위한 패키지
install.packages("slam")
install.packages("dplyr")
install.packages("readr") #파일을 읽어오기 위한 패키지

library(tm)
library(slam)
library(dplyr)
library(readr)
library(NLP4kec)

#형태소 분석기 실행하기
parsedData = text_parser(path = "/Users/kimnamyoun/TextConvert4TM/input/HomeApplication_cafe.xlsx"
                         ,language = "ko"
                         ,korDicPath = "./dictionary.txt")


##################################################################
#Text Pre-processing
##################################################################

#Corpus 생성
corp = VCorpus(VectorSource(parsedData))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#숫자 삭제
corp = tm_map(corp, removeNumbers)

#소문자로 변경
corp = tm_map(corp, tolower)

#특정 단어 삭제
corp = tm_map(corp, removeWords, c("있다", "하다","그렇다","되다","같다","가다","없다","보다","정도"))

#동의어 처리
for (j in seq(corp))
{
  corp[[j]] <- gsub("lg", "엘지", corp[[j]])
  corp[[j]] <- gsub("samsung", "삼성", corp[[j]])
}
##################################################################

#텍스트문서 형식으로 변환
corp = tm_map(corp, PlainTextDocument)

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))


#Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm = TermDocumentMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))

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

#맥북 사용자는 폰트 import하기
install.packages("extrafont")
library(extrafont)
#font_import()
loadfonts(device="postscript")

#단어 빈도수 바차트로 보여주기
ggplot(wordDf, aes(x=word, y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family = "AppleGothic"))

#단어 10개만 바차트로 보여주기
ggplot(head(wordDf,10), aes(x=word, y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family = "AppleGothic"))

#상위 20개 단어만 바차트로 보여주기
ggplot(head(arrange(wordDf,-freq),20), aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family = "AppleGothic"))


#Word Cloud 그리기
install.packages("wordcloud")
library(wordcloud)

pal = brewer.pal(n = 12, name = "Set2") # n:사용할 색깔 수, name:색깔 조합 이름
# http://colorbrewer2.org/ 참고

wordcloud(wordDf$word # 단어
          , wordDf$freq # 빈도수
          , min.freq = 5 # 표현할 단어의 최소 빈도수
          , colors = pal # 위에서 만든 팔레트 정보 입력
          , rot.per = 0.5 #회전한 단어 비율
          , random.order = F # 단어의 노출 순서 랜덤 여부 결정
          , scale = c(3,1) # scale값에서 앞에 값이 커야 빈도수가 큰 단어 사이즈가 커야함
          , family="AppleGothic") # 맥 폰트 설정


#treeMap 그리기
install.packages("treemap")
library(treemap)
treemap(wordDf # 대상 데이터 설정
        ,title = "Word Tree Map"
        ,index = c("word") # 박스 안에 들어갈 변수 설정
        ,vSize = "freq"  # 박스 크기 기준
        ,fontfamily.labels = "AppleGothic" # 맥 폰트 설정
        ,fontsize.labels = 12 # 폰트 크기 설정
        ,palette=pal # 위에서 만든 팔레트 정보 입력
        ,border.col = "white") # 경계선 색깔 설정
