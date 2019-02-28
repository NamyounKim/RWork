# word2vec 패키지 설치
install.packages("/Users/kimnamyoun/Downloads/wordVectors_2.0_mac.tgz", repos = NULL)
install.packages("경로명/wordVectors_2.0.zip", repo = NULL)
install.packages("tsne")

library(wordVectors)
library(tsne)
library(readr)

# 이전에 만들었던 형태소분석 결과를 가져옴
parsedData_df = readRDS("./raw_data/parsed_petition_data.RDS")


# 1. word2vec 모델링 -------------------------------------------------------------------------------------------------------------------------------------------------------------
# word2vec Train용 TXT파일 만들기
write.table(parsedData_df$text, file = "./Week_5/trainTxt.txt", row.names = FALSE, col.names = FALSE, quote = F)

#모델 Training
model = train_word2vec(train_file = "./Week_5/trainTxt.txt"
                       , threads=3
                       , vectors=100
                       , force = T
                       , window = 6
                       , output_file = "./Week_5/trainTxt.bin")

#word2vector model 확인하기
model

# 2. word2vec 결과 핸들링 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#</s> 삭제하기
model = model[rownames(model)!="</s>",]

# 한글자 단어 삭제하기
model = model[nchar(rownames(model))>1,]



# 3. word2vec에서 연관단어 추출 및 단어가 연산하기 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#연관 키워드 추출하기
nearest_to(model, model[["청원"]], 20)

#2가지 이상 키워드에 대한 연관 키워드 추출하기
nearest_to(model,model[[c("청원","청와대")]], 20)

#단어간 연산하기
subVec = model[rownames(model)=="청원",] - model[rownames(model) == "청와대",] + model[rownames(model) == "국회의원",]
nearest_to(model, subVec, 20)


# 4. word2vec 결과 시각화 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#전체 단어 관계 시각화
install.packages("extrafont")
library(extrafont)
par(family="AppleGothic")

plot(model)



# 5. 단어간 거리(유사도) 계산 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#Cosine 거리
cosineDist(model[["세월호"]], model[["침몰"]])
cosineDist(model[["세월호"]], model[["국회의원"]])

#Cosine 유사도 (= 1 - Cosine거리)
cosineSimilarity(model[["세월호"]], model[["침몰"]])
cosineSimilarity(model[["세월호"]], model[["국회의원"]])

#Euclidean Distance
dist(model[(row.names(model)=="세월호" | row.names(model)=="침몰"),])
dist(model[(row.names(model)=="세월호" | row.names(model)=="국회의원"),])





