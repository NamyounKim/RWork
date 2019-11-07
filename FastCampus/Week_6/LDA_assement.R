install.packages("Rmpfr")

library(topicmodels)
library(Rmpfr)



# 모델 평가 --------------------------------------------------------------------------------------------------------------------------------------------
harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
burnin = 0
keep = 1

# extract logliks from each topic
logLiks_many <- lapply(lda_model_paraller_31_50, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
sequ = seq(31, 50, by = 1)
plot(sequ, hm_many, type = "l")

# compute optimum number of topics
sequ[which.max(hm_many)]



# 5. LDA결과 시각화 하기 --------------------------------------------------------------------------------
lda_tm = lda_model_paraller_31_50[[20]]

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
