library(wordVectors)
library(dplyr)
library(tm)

# 속성명 정하기
attKeyword = c("지진","어린이집","성추행","아파트","취업","병원","학대")

# 단어간 코사인 유사도 행렬 구하기
simMat = cosineSimilarity(model, model)

# 속성별 가중치 행렬 만들기
weightMat = simMat[row.names(simMat) %in% attKeyword,]

# TDM 만들기 (TF-IDF기준) - corp변수를 만들어야함
corp =  readRDS("./raw_data/corpus_petition.RDS")
tdmW = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)
                                             ,weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

# TDM의 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
rownames(tdmW) = trimws(rownames(tdmW))
tdmW = tdmW[nchar(rownames(tdmW)) > 1,]

# TDM의 sparse term 삭제하기
tdmW = removeSparseTerms(tdmW, as.numeric(0.98))

# TDM을 행렬로 변환하기
tdmW_mat = as.matrix(tdmW)

# 가중치 행렬의 단어와 TDM 단어 맞추기
weightMat = weightMat[,colnames(weightMat) %in% rownames(tdmW_mat)]
tdmW_mat = tdmW_mat[rownames(tdmW_mat) %in% colnames(weightMat),]

# 단어 순서 맞추기
weightMat = weightMat[,sort(colnames(weightMat))]

# 단어 순서가 맞는지 확인
weightMat[,1:10]
tdmW_mat[1:10,1:10]

## TDM과 가중치 행렬의 내적(inner product) 구하기 - 각 문서에 속성 점수 만들기
attScore = weightMat %*% tdmW_mat
attScore = as.data.frame(attScore)
attScore = t(attScore)

# 각 문서별 스코어 확인 하기
textData =  readRDS("./raw_data/petitions_content_2018.RDS")
attScore[597,]
textData[597,"content"]

attScore[8855,]
textData[8855,"content"]

attScore[2110,]
textData[2110,"content"]

# 각 문서별 속성 스코어 붙이기
textData2 = cbind(textData, attScore)

# 각 문서별로 MAX 스코어와 속성값 붙이기
maxAttName = NULL
maxAttVal = NULL
for(i in 1:nrow(attScore)){
  maxAttName[i] = names(which.max(attScore[i,]))
  maxAttVal[i] = max(attScore[i,])
}
textData2 = cbind(textData2, maxAttName, maxAttVal)

# 전체 속성별 분류 개수 확인하기
tapply(textData2$title, textData2$maxAttName, length)

filter_data = textData2 %>% filter(maxAttName == "취업", maxAttVal > 0.6) %>% select(title, maxAttVal)



