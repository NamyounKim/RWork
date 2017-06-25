require(dplyr)
require(stringi)
require(data.table)

## Load model data
load(file="predict_svm_results_forVal.RData")

## 예측결과 -> modelResult 변수 사용

##실제평가 Load
rawValidation <- read.csv("/home/ruser/TextPrism/Blog_ValidationSet_NoContent.csv",
                       header=TRUE, fileEncoding="UTF-8")

##예측결과와 실제결과 Mapping
forValidation <- select(merge(rawValidation, modelResult, by="crawl_data_id"),
                    crawl_data_id, spam_yn, pred)

##오분류표 작성
misClassification <- addmargins(table(forValidation$spam_yn, forValidation$pred))

print(misClassification)

#write.csv(modelResult,file="./modelResult/modelResult_0518.csv",row.names=FALSE)

save.image(file="validate_results.RData")
