library(wordVectors)
library(tsne)
library(ggplot2)

##### word2vec 만들기 #####
raw_review_weakness$parsedContent = parsed_weakness
inputData = raw_review_weakness %>% filter(brand_nm == "마몽드")

write.table(inputData$parsedContent, file = "./review.txt", row.names = F, col.names = F, quote = F)

model = train_word2vec(train_file = "./review.txt"
                       , threads = 10
                       , vectors = 100
                       , window = 8
                       )
model
model = model[rownames(model) != "</s>",]
model = model[nchar(rownames(model)) > 1,]
par(mfrow=c(1,1))
plot(model)

nearest_to(model, model[[c("뭉치다")]], 60)
inputData$content[grepl("뭉치다",inputData$parsedContent)]

cosineSimilarity(model[["좋다"]], model[["보습"]])
