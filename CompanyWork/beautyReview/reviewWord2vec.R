library(wordVectors)
library(tsne)
library(ggplot2)
library(dplyr)

source("./tm_function.R")
source("../../ggRader.R")
source("../../ggRader2.R")

##### word2vec 만들기 #####
raw_review_strength$parsedContent = parsed_strength
raw_review_weakness$parsedContent = parsed_weakness
#inputData = raw_review_strength %>% filter(brand_nm == "이니스프리 본품")

write.table(raw_review_strength$parsedContent, file = "./strength_review.txt", row.names = F, col.names = F, quote = F)
write.table(raw_review_weakness$parsedContent, file = "./weakness_review.txt", row.names = F, col.names = F, quote = F)

strength_model = train_word2vec(train_file = "./strength_review.txt"
                               , threads = 10
                               , vectors = 100
                               , window = 4)

strength_model = strength_model[rownames(strength_model) != "</s>",]
strength_model = strength_model[nchar(rownames(strength_model)) > 1,]

weakness_model = train_word2vec(train_file = "./weakness_review.txt"
                                , threads = 10
                                , vectors = 100
                                , window = 4)

weakness_model = weakness_model[rownames(weakness_model) != "</s>",]
weakness_model = weakness_model[nchar(rownames(weakness_model)) > 1,]

#plot(model)

nearest_to(weakness_model, weakness_model[["휴대"]], 30)
nearest_to(model, model[[c("세정력","클렌징","말끔하다")]], 30)
cosineSimilarity(model[["밀착력"]], model[["지속력"]])


#단어간 코사인 유사도 매트릭스 구하기
strength_cosSim = cosineSimilarity(strength_model, strength_model)
weakness_cosSim = cosineSimilarity(weakness_model, weakness_model)

#토픽(속성) 키워드 정하기
thesaurus = read_csv("../dic/thesaurusForBeauty", col_names = T)
attKeyword = thesaurus$word

#TDM 만들기
strength_tdm = makeDtm(raw_review_strength %>% select(parsedContent), 0.999, "tdm-tf")
strength_tdmMat = as.matrix(strength_tdm)

weakness_tdm = makeDtm(raw_review_weakness %>% select(parsedContent), 0.999, "tdm-tf")
weakness_tdmMat = as.matrix(weakness_tdm)
rm(strength_tdm, weakness_tdm)

#속성 스코어 구하기
strength_att = getAttributeScore(strength_cosSim, strength_tdmMat, attKeyword, cutOff = 0.5)
weakness_att = getAttributeScore(weakness_cosSim, weakness_tdmMat, attKeyword, cutOff = 0.5)

strength_score = strength_att$docScore
weakness_score = weakness_att$docScore

# 최종결과 정리하기
a1 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a1",]$word)))
a2 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a2",]$word)))
a3 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a3",]$word)))
a4 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a4",]$word)))
a5 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a5",]$word)))
a6 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a6",]$word)))
a7 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a7",]$word)))
a8 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a8",]$word)))
a9 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a9",]$word)))
a10 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a10",]$word)))
a11 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a11",]$word)))
a12 = rowMeans(strength_score %>% select(which(colnames(strength_score) %in% thesaurus[thesaurus$attCd == "a12",]$word)))

strength_score = cbind(strength_score, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
strength_score = strength_score %>% select(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
boxplot(strength_score)
#pairs(docScore)

# 최종결과 정리하기
a1 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a1",]$word)))
a2 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a2",]$word)))
a3 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a3",]$word)))
a4 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a4",]$word)))
a5 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a5",]$word)))
a6 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a6",]$word)))
a7 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a7",]$word)))
a8 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a8",]$word)))
a9 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a9",]$word)))
a10 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a10",]$word)))
a11 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a11",]$word)))
a12 = rowMeans(weakness_score %>% select(which(colnames(weakness_score) %in% thesaurus[thesaurus$attCd == "a12",]$word)))

weakness_score = cbind(weakness_score, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
weakness_score = weakness_score %>% select(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
boxplot(weakness_score)

#++++검증++++#
strength_score[1001:1005,]
raw_review_strength$content[1001:1005]
raw_review_strength$parsedContent[1001:1005]

head(sort(strength_att$funcSim["휴대",], decreasing = T), 30) # 해당 속성의 주요 유사단어

targetDocNo = 1003
strength_att$funcSim %*% strength_att$sub_tdmMat[,targetDocNo] # 해당 문서의 각 속성 키워드별 점수
strength_att$sub_tdmMat[(strength_att$sub_tdmMat[, targetDocNo] > 0), targetDocNo]
strength_att$funcSim["휴대", which(strength_att$sub_tdmMat[,targetDocNo] > 0)] #해당 문서의 출현단어 중 특정 속성의 유사단어 점수
#+++++++++++#


# 각 속성별 0~1 사이 값으로 scaling
strength_score = apply(strength_score, 2, function(x){(x-min(x))/(max(x)-min(x))})
weakness_score = apply(weakness_score, 2, function(x){(x-min(x))/(max(x)-min(x))})

#원문에 붙이기
raw_review_strength = cbind(raw_review_strength, strength_score)
raw_review_weakness = cbind(raw_review_weakness, weakness_score)


# 브랜드 별 평 산출
raw_review_strength %>% group_by(brand_nm) %>% dplyr::summarise(n=n()) %>% arrange(-n)

# 평점 구간 정하기
attName = colnames(strength_score)[1:10]
par(mfrow=c(2,5))
for(i in 1:10){
  aname = attName[i]
  temp = rawData %>% filter(product =="페이셜 마일드 필링") %>% select(aname)
  barplot(quantile(temp[,1], seq(0,1,0.2)), xlim = c(0,1), horiz = T, main = aname)
}

