library(tm)

#아래 코드를 수행하기 이전에 반드시 DTM을 생성해야 합니다.
#text_handling.R 소스코드 참고 하세요.
corp = readRDS("./data/corpus_petition.RDS")


#TF-IDF 값으로 연관 키워드 추출하기
dtm_tfidf = DocumentTermMatrix(corp
                               ,control=list(wordLengths=c(2,Inf)
                               ,weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

dtm_tfidf = removeSparseTerms(dtm_tfidf, sparse = 0.98)

findAssocs(dtm_tfidf, "아파트", 0.05)

# 직접 단어간 상관관계 구하기
dtm_mat = as.matrix(dtm_tfidf)
cor_term = cor(dtm_mat)
cor_ref = cor_term[,"청원"]
cor_ref


