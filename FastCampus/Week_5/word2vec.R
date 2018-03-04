#======================================
# word2vec로 연관 키워드 추출하기
#======================================
install.packages("/Users/kimnamyoun/GitHub/wordVectors_2.0.tgz", repos = NULL, type = "source")
install.packages("tsne")
library(wordVectors)
library(tsne)
library(readr)

# 이전에 만들었던 corpus를 다시 리스트 형태로 변경
targetData = NULL
for(i in 1:length(corp)){
  temp = corp[[i]]$content
  targetData[i] = temp
}

#word2vec Train용 TXT파일 만들기
write.table(targetData, file = "./trainTxt.txt", row.names = FALSE, col.names = FALSE, quote = F)

#모델 Training
model = train_word2vec(train_file = "./trainTxt.txt"
                       , threads=3
                       , vectors=100
                       , force = T
                       , window = 6)

#word2vector model 확인하기
model

#</s> 삭제하기
model = model[rownames(model)!="</s>",]

# 한글자 단어 삭제하기
model = model[nchar(rownames(model))>1,]

#연관 키워드 추출하기
nearest_to(model,model[["청원"]], 20)

#2가지 이상 키워드에 대한 연관 키워드 추출하기
nearest_to(model,model[[c("청원","청와대")]], 20)

#단어간 연산하기
subVec = model[rownames(model)=="청원",] - model[rownames(model) == "청와대",] + model[rownames(model) == "국회의원",]
nearest_to(model, subVec, 20)

#전체 단어 관계 시각화
install.packages("extrafont")
library(extrafont) 
par(family="AppleGothic")

plot(model, ... = )

#Cosine 거리
cosineDist(model[["청원"]], model[["청와대"]])
cosineDist(model[["청원"]], model[["국회의원"]])

#Cosine 유사도 (= 1 - Cosine거리)
cosineSimilarity(model[["청원"]], model[["청와대"]])
cosineSimilarity(model[["청원"]], model[["국회의원"]])

#Euclidean Distance
dist(model[(row.names(model)=="청원" | row.names(model)=="청와대"),])
dist(model[(row.names(model)=="청원" | row.names(model)=="국회의원"),])
