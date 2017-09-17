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

#형태소 분석기 실행하기
parsedData = text_parser(path = "./HomeApplication_cafe.xlsx"
                         ,language = "ko"
                         ,korDicPath = "./dictionary.txt")


#word2vec Train용 TXT파일 만들기
write.table(targetData$parsedContent, file = "./trainTxt.txt", row.names = FALSE, col.names = FALSE, quote = F)

#모델 Training
model = train_word2vec("./trainTxt.txt", output_file = "w2vModel.bin", 
                       threads=3, vectors=100, force = T)

#word2vector 확인하기
read.vectors("./w2vModel.bin")

#연관 키워드 추출하기
nearest_to(model,model[["냉장고"]], 20)

#2가지 이상 키워드에 대한 연관 키워드 추출하기
nearest_to(model,model[[c("냉장고","양문")]], 20)

#단어간 연산하기
subVec = model[rownames(model)=="냉장고",] - model[rownames(model) == "디오스",] + model[rownames(model) == "트롬",]
nearest_to(model, subVec, 20)

#전체 단어 관계 시각화
library(extrafont) 
par(family="AppleGothic") 

plot(model)

######### 전처리 하기 ############
#영어 소문자로 바꾸기
targetData$parsedContent = tolower(targetData$parsedContent)




#Cosine 유사도 (= 1 - Cosine거리)
cosineSimilarity(model[["냉장고"]], model[["그룹"]])

#Euclidean Distance
dist(model[(row.names(model)=="냉장고" | row.names(model)=="그룹"),])

