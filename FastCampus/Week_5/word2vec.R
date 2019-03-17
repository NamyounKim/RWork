# word2vec 패키지 설치
install.packages("/Users/kimnamyoun/Downloads/wordVectors_2.0_mac.tgz", repos = NULL)
install.packages("경로명/wordVectors_2.0.zip", repo = NULL)
install.packages("tsne")

library(wordVectors)
library(tsne)
library(readr)

# 이전에 만들었던 형태소분석 결과를 가져옴
parsedData_df = readRDS("./raw_data/parsed_petition_data.RDS")
stopWordDic = read_csv("./dictionary/stopword_ko.csv")

# 1. word2vec 모델링 -------------------------------------------------------------------------------------------------------------------------------------------------------------
# word2vec Train용 TXT파일 만들기
write.table(parsedData_df$text, file = "./Week_5/trainTxt.txt", row.names = FALSE, col.names = FALSE, quote = F)

#모델 Training
model = train_word2vec(train_file = "./Week_5/trainTxt.txt"
                       , threads=3
                       , vectors=100
                       , force = T
                       , window = 6
                       , output_file = "./Week_5/word2vec_model.bin")

#word2vector model 확인하기
model

# 파일로 저장된 word2vec모델 읽어오기
model = wordVectors::read.binary.vectors("./Week_5/word2vec_model.bin")

# 2. word2vec 결과 핸들링 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#</s> 삭제하기
model = model[rownames(model)!="</s>",]

# 한글자 단어 삭제하기
model = model[nchar(rownames(model))>1,]

# 불용어 삭제하기
model = model[!(rownames(model) %in% stopWordDic$stopword),]
model

# 3. word2vec에서 연관단어 추출 및 단어가 연산하기 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#연관 키워드 추출하기
nearest_to(model, model[["어린이집"]], 20)

#2가지 이상 키워드에 대한 연관 키워드 추출하기
nearest_to(model,model[[c("어린이집","원장")]], 20)

#단어간 연산하기
subVec = model[rownames(model)=="어린이집",] - model[rownames(model) == "원장",] + model[rownames(model) == "초등학교",]
nearest_to(model, subVec, 20)


# 4. word2vec 결과 시각화 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#전체 단어 관계 시각화
install.packages("extrafont")
library(extrafont)
par(family="AppleGothic")

plot(model)



# 5. 단어간 거리(유사도) 계산 -------------------------------------------------------------------------------------------------------------------------------------------------------------
#Cosine 거리
cosineDist(model[["어린이집"]], model[["초등학교"]])
cosineDist(model[["어린이집"]], model[["원장"]])

#Cosine 유사도 (= 1 - Cosine거리)
cosineSimilarity(model[["어린이집"]], model[["초등학교"]])
cosineSimilarity(model[["어린이집"]], model[["원장"]])

#Euclidean Distance
dist(model[(row.names(model)=="어린이집" | row.names(model)=="초등학교"),])
dist(model[(row.names(model)=="어린이집" | row.names(model)=="원장"),])





