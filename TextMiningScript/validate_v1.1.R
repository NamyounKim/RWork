rm(list=ls())
gc()

start.time <- Sys.time()

libraryList <- c("dplyr","stringi","tm","caret","data.table","reshape","RODBC","RODBCext")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
require(dplyr)
require(stringi)
require(tm)
require(data.table)
require(reshape)
require(RODBC)
require(RODBCext)
#require(e1071)

print("===== This is SPAM ML Validation Program =====")

############################################
## Road ML Model
############################################
load(file="/home/ruser/TextPrism/modelResult/model_#test_0.997.RData")
source("/home/ruser/TextPrism/RSource/ML_functions.R")

##### Option #####
tfIdf <- FALSE
sparseRatio <- 0.997

############################################
## Read in TM results
############################################
print("Read TP Result from DB")
conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')

tpResult<-sqlQuery(conn,'SELECT crawl_data_id, crawled_date, keyword, ranking as count, role FROM trendtracker.t_tp_result_rank_train WHERE user="ml_val";')

#odbcClose(conn)
print("DB Reading END")
odbcClose(conn)

corpus <- fn_tm_keys(tpResult)
#tmValidationRoles <- fn_tm_roles(validationTm)
#tmValidationDocType <- fn_tm_doc_type(validationTm)

print(paste("Target Document :",nrow(corpus)))
  
##### Make DTM #####
ifelse(tfIdf==TRUE , dtmDf <- fn_makeTfIdfDTM(corpus, sparseRatio), dtmDf <- fn_makeDTM(corpus, sparseRatio))

## Remove superfluous var columns
notUseTermVector <- setdiff(names(dtmDf), names(noDocidTotal))
dtmDf <- dtmDf[, !(names(dtmDf) %in% notUseTermVector)]

useTermVector <- setdiff(names(subset(noDocidTotal, select=(-spam_yn))), names(dtmDf))
forAdd <- data.frame(matrix(0, ncol=length(useTermVector), nrow=nrow(dtmDf)))
names(forAdd) <- useTermVector

## use data.table because data.frame causes memory overflow
dtmDt <- data.table(dtmDf)
forAddDt <- data.table(forAdd)
forPredictDtm <- cbind(dtmDt, forAddDt)

## Predict with model
print("Start Prediction")
predSvm <- predict(spamModel, forPredictDtm)
predSvmProb <- predict(spamModel, forPredictDtm, type = "prob")

predResult <- data.frame(crawl_data_id=corpus$crawl_data_id, pred=predSvm)
predResultProb <- data.frame(crawl_data_id=corpus$crawl_data_id, pred=predSvmProb)
predResult <- merge(predResult, predResultProb, by="crawl_data_id")

print("End Prediction")
print(summary(predResult))

print("Start Validation")
##실제평가 Load
rawValidation <- read.csv("/home/ruser/TextPrism/Blog_ValidationSet_NoContent.csv",
                       header=TRUE, fileEncoding="UTF-8")
##예측결과와 실제결과 Mapping
forValidation <- subset(merge(rawValidation, predResult, by="crawl_data_id"), 
                        select = c(crawl_data_id,spam_yn,pred))

##오분류표 작성
misClassification <- addmargins(table(forValidation$spam_yn, forValidation$pred))

##Confuse Matrix
confusionMatrix(forValidation$pred, forValidation$spam_yn, positive = "spam")

print(misClassification)
print("End Validation")

end.time <- Sys.time()
predictTime <- end.time - start.time

print("Working Time: ")
print(predictTime)
print("Validation Complete")

rm(tpResult)
rm(corpus)
#rm(tmValidationRoles)
#rm(tmValidationDocType)

#save.image(file="predict_svm_results_newSixPocket4.RData")
save.image(file="validation_svm_results.RData")
