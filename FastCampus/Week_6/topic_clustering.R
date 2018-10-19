install.packages("topicmodels")
install.packages("LDAvis")
install.packages("servr")

library(topicmodels)
library(LDAvis)
library(servr)
library(readr)
library(tm)
library(slam)
library(dplyr)
library(NLP4kec)

# 1. 원문 데이터 및 사전 불러오기 ----------------------------------------------------------------------------------------------------
textData = readRDS("./raw_data/petitions_content_2018.RDS")

#동의어 / 불용어 사전 불러오기
stopWordDic = read_csv("./dictionary/stopword_ko.csv")
synonymDic = read_csv("./dictionary/synonym.csv")


# 2. 형태소 분석 및 전처리------------------------------------------------------------------------------------------------------------
#형태소 분석기 실행하기
parsedData = r_parser_r(textData$content, language = "ko", useEn = T, korDicPath = "./dictionary/user_dictionary.txt")
parsedData = readRDS("./raw_data/parsed_data.RDS")

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

#텍스트문서 형식으로 변환
corp = tm_map(corp, PlainTextDocument)

# 3. DTM 생성 및 Sparse Term 삭제 ----------------------------------------------------------------------------------------------------------
corp = readRDS("./raw_data/corpus.RDS")

#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

#Sparse Terms 삭제
dtm = removeSparseTerms(dtm, as.numeric(0.997))
dtm

## LDA 할 때 DTM 크기 조절
#단어별 Tf-Idf 값 구하기
term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
term_tfidf

#박스그래프로 분포 확인
boxplot(term_tfidf, outline = T)
quantile(term_tfidf, seq(0, 1, 0.25))

# Tf-Idf 값 기준으로 dtm 크기 줄여서 new_dtm 만들기
new_dtm = dtm[,term_tfidf >= 0.1]
new_dtm = new_dtm[row_sums(new_dtm) > 0,]
new_dtm


# 4. Running LDA ----------------------------------------------------------------------------------------------------------------
#분석명, 랜덤 seed, 클러스트 개수 setup
name = "petition"
SEED = 100
k = 20 #클러스터 개수 세팅

#LDA 실행
lda_tm = LDA(new_dtm, control=list(seed=SEED), k)

#토픽별 핵심단어 저장하기
term_topic = terms(lda_tm, 30)
term_topic

#문서별 토픽 번호 저장하기
doc_topic = topics(lda_tm, 1)
doc_topic_df = as.data.frame(doc_topic)
doc_topic_df$rown = as.numeric(row.names(doc_topic_df)) # 조인키 만들기

#문서별 토픽 확률값 계산하기
doc_Prob = posterior(lda_tm)$topics
doc_Prob_df = as.data.frame(doc_Prob)

#최대 확률값 찾기
doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max) #--> 행기준으로 max값을 찾겠다.

#문서별 토픽번호 및 확률값 추출하기
doc_Prob_df$rown = doc_topic_df$rown
parsedData = as.data.frame(parsedData)
parsedData$rown = as.numeric(row.names(parsedData))

id_topic = merge(doc_topic_df, doc_Prob_df, by="rown")
id_topic = merge(id_topic, parsedData, by="rown", all.y = TRUE)

id_topic = id_topic %>% select(rown, parsedData, doc_topic, maxProb) #코드 변경됨
id_topic$content = textData$content

#단어별 토픽 확률값 출력하기
posterior(lda_tm)$terms

#토픽별 핵심 단어 파일로 출력하기
filePathName = paste0("./LDA_output/",name,"_",k,"_LDA_Result.csv")
write.table(term_topic, filePathName, sep=",", row.names=FALSE)

#문서별 토픽 번호 및 확률값 출력하기
filePathName = paste0("./LDA_output/",name,"_",k,"_DOC","_LDA_Result.csv",sep="")
write.table(id_topic, filePathName, sep=",", row.names=FALSE)


# 5. LDA결과 시각화 하기 --------------------------------------------------------------------------------
# phi는 각 단어별 토픽에 포함될 확률값 입니다.
phi = posterior(lda_tm)$terms %>% as.matrix

# theta는 각 문서별 토픽에 포함될 확률값 입니다.
theta = posterior(lda_tm)$topics %>% as.matrix

# vocab는 전체 단어 리스트 입니다.
vocab = colnames(phi)

# 각 문서별 문서 길이를 구합니다.
doc_length = vector()
doc_topic_df=as.data.frame(doc_topic)

for( i in as.numeric(row.names(doc_topic_df))){
  temp = corp[[i]]$content
  doc_length = c(doc_length, nchar(temp[1]))
}

# 각 단어별 빈도수를 구합니다.
new_dtm_m = as.matrix(new_dtm)
freq_matrix = data.frame(ST = colnames(new_dtm_m),
                          Freq = colSums(new_dtm_m))

# 위에서 구한 값들을 파라메터 값으로 넘겨서 시각화를 하기 위한 데이터를 만들어 줍니다.
source("./Week_6/createJsonForChart_v2.R")
json_lda = createJson(phi = phi
                      , theta = theta,
                          vocab = vocab,
                          doc.length = doc_length,
                          term.frequency = freq_matrix$Freq,
                          #mds.method = jsPCA #canberraPCA가 작동 안할 때 사용
                          mds.method = canberraPCA
)

# 톰캣으로 보내기
serVis(json_lda, out.dir = paste("C:/tomcat8_33/webapps/",name,"_",k,sep=""), open.browser = FALSE)
serVis(json_lda, open.browser = T) # MAC인 경우

# 예시 URL
#localhost:8080/petition_LDA_15
