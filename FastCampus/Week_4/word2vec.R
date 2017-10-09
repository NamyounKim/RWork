#======================================
# word2vec로 연관 키워드 추출하기
#======================================
install.packages("tsne")
install.packages("devtools")
library(devtools)   #Rtools는 Windows에 깔때 별도로 깔아야 한다.
install_github("bmschmidt/wordVectors")
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
                       , window = 10)

#word2vector model 확인하기
model

#</s> 삭제하기
model = model[rownames(model)!="</s>",]

#연관 키워드 추출하기
nearest_to(model,model[["냉장고"]], 20)

#2가지 이상 키워드에 대한 연관 키워드 추출하기
nearest_to(model,model[[c("냉장고","양문")]], 20)

#단어간 연산하기
subVec = model[rownames(model)=="냉장고",] - model[rownames(model) == "디오스",] + model[rownames(model) == "트롬",]
nearest_to(model, subVec, 20)

#전체 단어 관계 시각화
install.packages("extrafont")
library(extrafont) 
par(family="AppleGothic") 

plot(model)

#Cosine 거리
cosineDist(model[["김치"]], model[["김치냉장고"]])
cosineDist(model[["김치"]], model[["세탁기"]])

#Cosine 유사도 (= 1 - Cosine거리)
cosineSimilarity(model[["김치"]], model[["김치냉장고"]])
cosineSimilarity(model[["김치"]], model[["세탁기"]])

#Euclidean Distance
dist(model[(row.names(model)=="김치" | row.names(model)=="김치냉장고"),])
dist(model[(row.names(model)=="김치" | row.names(model)=="세탁기"),])
