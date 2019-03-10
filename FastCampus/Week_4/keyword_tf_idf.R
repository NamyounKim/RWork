library(tm)

#아래 코드를 수행하기 이전에 반드시 DTM을 생성해야 합니다.
#text_handling.R 소스코드 참고 하세요.
corp = readRDS("./raw_data/corpus_petition.RDS")
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
dtm = removeSparseTerms(dtm, as.numeric(0.98))

# 1. 연관 키워드 추출 및 TF-IDF 가중치 ------------------------------------------------------------------------------------------------------------------------------------
# "청원"의 연관 키워드 구하기
findAssocs(dtm, terms = "청원", corlimit = 0.1)

# 직접 단어간 상관관계 구하기
dtm_m = as.matrix(dtm)
cor_term = cor(dtm_m)
cor_ref = cor_term[,"청원"]
cor_ref

#TF-IDF 값으로 연관 키워드 추출하기
dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                            weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtmW) = trimws(colnames(dtmW))
dtmW = dtmW[,nchar(colnames(dtmW)) > 1]

dtmW = removeSparseTerms(dtmW, as.numeric(0.98))

findAssocs(dtmW, "청원", 0.05)

