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
library(reshape2)
library(NLP4kec)

new_dtm = target_tf %>% filter(prd_ltyp_nm_n =="메이크업") %>% select(34:ncol(target_tf))
new_dtm = new_dtm %>% select(which(colSums(skin_dtmW) !=0.0))
new_dtm = new_dtm %>% filter(rowSums(skin_dtmW) != 0)


dtm2 = removeSparseTerms(dtm, 0.999)

## LDA 할 때 DTM 크기 조절
#단어별 Tf-Idf 값 구하기
term_tfidf = tapply(dtm2$v/row_sums(dtm2)[dtm2$i], dtm2$j, mean) * log2(nDocs(dtm2)/col_sums(dtm2 > 0))

#박스그래프로 분포 확인
boxplot(term_tfidf, outline = F)

# Tf-Idf 값 기준으로 dtm 크기 줄여서 new_dtm 만들기
new_dtm = dtm2[,term_tfidf >= 1]
new_dtm = new_dtm[row_sums(new_dtm) > 0,]

############################################
## Running LDA
############################################
#분석명, 랜덤 seed, 클러스트 개수 setup
name = "apMallReview"
SEED = 2017
k = 10 #클러스터 개수 세팅

#LDA 실행
lda_tm = LDA(new_dtm, control=list(seed=SEED), k)

#토픽별 핵심단어 저장하기
term_topic = terms(lda_tm, 30)

#토픽별 핵심 단어 파일로 출력하기
filePathName = paste0("./LDA_output/",name,"_",k,"_LDA_Result.csv")
write.table(term_topic, filePathName, sep=",", row.names=FALSE)

#문서별 토픽 번호 저장하기
doc_topic = topics(lda_tm, 1)
doc_topic_df = as.data.frame(doc_topic)
doc_topic_df$rown = as.numeric(row.names(doc_topic_df))

#문서별 토픽 확률값 계산하기
doc_Prob = posterior(lda_tm)$topics
doc_Prob_df = as.data.frame(doc_Prob)

#최대 확률값 찾기
doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max)

#문서별 토픽번호 및 확률값 추출하기
doc_Prob_df$rown = doc_topic_df$rown
parsedData = as.data.frame(parsedData)
parsedData$rown = as.numeric(row.names(parsedData))
id_topic = merge(doc_topic_df, doc_Prob_df, by="rown")
id_topic = merge(id_topic, parsedData, by="rown", all.y = TRUE)
id_topic = subset(id_topic,select=c("rown","parsedData","doc_topic","maxProb"))

#문서별 토픽 번호 및 확률값 출력하기
filePathName = paste0("./LDA_output/",name,"_",k,"_DOC","_LDA_Result.csv",sep="")
write.table(id_topic, filePathName, sep=",", row.names=FALSE)

#단어별 토픽 확률값 출력하기
term_topic_prob = posterior(lda_tm)$terms
term_topic_prob
word = NULL
topic = NULL
maxVal = NULL
for(i in 1:ncol(term_topic_prob)){
  word[i] = colnames(term_topic_prob)[i]
  topic[i] = which.max(term_topic_prob[,colnames(term_topic_prob)[i]])
  maxVal[i] = term_topic_prob[,colnames(term_topic_prob)[i]]
}
term_topic_prob_max = data.frame(word, topic, maxVal)
temp = dcast(term_topic_prob_max, word~topic, fun.aggregate = sum, value.var = "maxVal")

#########################################
## Make visualization
#########################################

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
source("./Week_5/createJsonForChart_v2.R")
json_lda = createJson(phi = phi, theta = theta,
                      vocab = vocab,
                      doc.length = doc_length,
                      term.frequency = freq_matrix$Freq,
                      #mds.method = jsPCA #canberraPCA가 작동 안할 때 사용
                      mds.method = canberraPCA
)

# 톰캣으로 보내기
serVis(json_lda, out.dir = paste("C:/tomcat8/webapps/",name,"_",k,sep=""), open.browser = FALSE)
serVis(json_lda, open.browser = T) # MAC인 경우 