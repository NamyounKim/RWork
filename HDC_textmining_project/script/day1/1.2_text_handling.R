install.packages("tm") #텍스트 마이닝을 위한 패키지
install.packages("slam")
#install.packages("dplyr")
#install.packages("readr") #파일을 읽어오기 위한 패키지

library(tm)
library(slam)
library(dplyr)
library(NLP4kec)
library(readr)

# 1. 원문 데이터 및 사전 불러오기 --------------------------------------------------------------------------------------------------------------------------------------------
textData = readRDS("./data/petitions_content_2018.RDS")

#동의어 / 불용어 사전 불러오기
stopword_dic = read_csv("./dictionary/stopword_ko.csv")
synonym_dic = read_csv("./dictionary/synonym.csv")


# 2. 형태소 분석 및 전처리----------------------------------------------------------------------------------------------------------------------------------------------------
#명사, 동사, 형용사만 추출
parsed_vec = r_parser_r(contentVector = textData$content
                        ,language = "ko"
                        ,useEn = T
                        ,korDicPath = "./dictionary/user_dictionary.txt")

# 명사만 추출
parsed_vec_noun = r_extract_noun(contentVector = textData$content
                                 ,language = "ko"
                                 ,useEn = T
                                 ,korDicPath = "./dictionary/user_dictionary.txt")

# 동의어 처리
parsed_vec = synonym_processing(parsedVector = parsed_vec
                                ,synonymDic = synonym_dic)


# Corpus에 doc_id를 추가하기 위한 데이터 프레임 만들기
parsed_df = data.frame(doc_id = textData$doc_id
                      ,text = parsed_vec)

saveRDS(parsed_df, file = "./data/parsed_petition_df.RDS") # 나중 재사용을 위해 저장

#Corpus 생성
corp = VCorpus(DataframeSource(parsed_df))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#숫자 삭제
corp = tm_map(corp, removeNumbers)

#특정 단어 삭제
corp = tm_map(corp, removeWords, stopword_dic$stopword)

saveRDS(corp, file = "./data/corpus_petition.RDS") # 나중 재사용을 위해 저장


# 3. DTM 생성 및 Sparse Term 삭제 --------------------------------------------------------------------------------------------------------------------------------------------------
#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))


#Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
tdm = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))

#Sparse Terms 삭제 (값이 작아질 수록 term수가 줄어든다.)
dtm = removeSparseTerms(dtm, sparse = 0.98)

dtm_mat = as.matrix(dtm)
write_csv(as.data.frame(dtm_mat), "./dtm_mat.csv")

#단어 발생 빈도 구하기
colSums(as.matrix(dtm))
word_freq = colSums(as.matrix(dtm))

#DTM을 데이터 프레임 형식으로 저장하기
dtm_df = as.data.frame(as.matrix(dtm))


# 4. 단어 빈도 정보 추출하기 --------------------------------------------------------------------------------------------------------------------------------------------------
#단어 개수 구하기
length(word_freq)


#단어 빈도 정보 Data Frame 만들기
word_df = data.frame(word = names(word_freq)
                     ,freq = word_freq)

#내림차순으로 단어 10개, sorting 하기
word_df %>% arrange(-freq) %>% head(10)


# 5. 단어 빈도 정보로 시각화 하기 ----------------------------------------------------------------------------------------------------------------------------------------------
#단어 빈도 시각화
library(ggplot2)

#상위 단어 10개만 바차트로 보여주기
top10 = word_df %>% arrange(-freq) %>% head(100) #상위 10개 단어만 추출
ggplot(top10, aes(x=word, y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family = "AppleGothic"))

#상위 10개 단어 빈도순으로 정렬하여 바차트로 보여주기
ggplot(top10, aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family = "AppleGothic"))


# Word Cloud 그리기
install.packages("wordcloud2")
library(wordcloud2)

top100 = word_df %>% top_n(100) # 상위 100개 단어만 추출

wordcloud2(data = top100
           , color = "random-light"
           #,color = "random-dark"
           ,shape = "diamond"
           ,size = 0.5
           ,fontFamily = "나눔고딕")

#circle, cardioid, diamond, triangle-forward, triangle, pentagon, star




# 6. TF-IDF 값으로 DTM 만들기 ----------------------------------------------------------------------------------------------------------------------------------------------
dtm_tfidf = DocumentTermMatrix(corp
                               ,control=list(wordLengths=c(2,Inf)
                               ,weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

dtm_tfidf = removeSparseTerms(dtm_tfidf, sparse = 0.98)

