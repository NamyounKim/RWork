library("devtools")
#Rtools는 Windows에 깔때 별도로 깔아야 한다.
install_github("bmschmidt/wordVectors")
library("wordVectors")
install.packages("tsne")
library("tsne")

model = train_word2vec("C:/develop/TextConvert4TM/news.txt",output="news_vectors2.bin",threads = 3, vectors = 150, window=12)
read.vectors("./news_vectors2.bin")
nearest_to(model,model[["이재명"]],20)
some = nearest_to(model,model[[c("박근혜","세월호")]],10)
plot(filter_to_rownames(model,names(some)))

plot(model)


model2 = train_word2vec("C:/develop/TextConvert4TM/out.txt",output="news_vectors3.bin",threads = 3, vectors = 150, window=12)
read.vectors("./news_vectors2.bin")
nearest_to(model2,model2[["최순실"]],20)
some2 = nearest_to(model2,model2[[c("최순실","세월호")]],50)
plot(filter_to_rownames(model2,names(some2)))

plot(model2)
model2[["박근혜"]] -  model2[["세월호"]]

#Euclidean Distance
dist(model2[(row.names(model2)=="세월호" | row.names(model2)=="박근혜"),])

#Cosine 유사도
cosineSimilarity(model2[["박근혜"]], model2[["세월호"]])
