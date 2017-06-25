rm(list=ls())
gc()

start.time <- Sys.time()

load(file="/home/ruser/TextPrism/modelResult/model_#test_0997.RData")
source("/home/ruser/TextPrism/RSource/ML_functions.R")

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
require(caret)
require(data.table)
require(reshape)
require(RODBC)
require(RODBCext)
#require(e1071)

############################################
## Read in TM results
############################################
print("Read TP Result from DB")
conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')

#validationTm<-sqlQuery(conn,'SELECT crawl_data_id, crawled_date, keyword, ranking as count, role FROM trendtracker.t_tp_result_rank_train WHERE user="ml_val";')
tpResult<-sqlQuery(conn,'SELECT crawl_data_id, keyword, ranking as count, role FROM trendtracker.t_tp_result_rank WHERE user="salaryMan";')

#odbcClose(conn)
print("DB Reading END")
user<-"test"

corpus <- fn_tm_keys(tpResult)
#tmValidationRoles <- fn_tm_roles(validationTm)
#tmValidationDocType <- fn_tm_doc_type(validationTm)

print(paste("Target Document :",nrow(corpus)))

dCorpus <- NULL
increment <- 30000
endRowNumCorpus <- nrow(corpus)
for(i in seq(from=1, to=nrow(corpus), by=increment)){
  print(paste(i," ~ ",(i+(increment-1)),sep=""))
  if((i+(increment-1)) <= nrow(corpus)){
    dCorpus <- corpus[i:(i+(increment-1)),]
  }
  else{
    dCorpus <- corpus[i:endRowNumCorpus,]
  }

  ## Make DTM
  dtmDf <- fn_makeDTM(dCorpus,0.997)

  ## Remove superfluous var columns
  notUseTermVector <- setdiff(names(dtmDf), names(noDocidTotal))
  dtmDf <- dtmDf[, !(names(dtmDf) %in% notUseTermVector)]

  useTermVector <- setdiff(names(select(noDocidTotal, -spam_yn)), names(dtmDf))
  forAdd <- data.frame(matrix(0, ncol=length(useTermVector), nrow=nrow(dtmDf)))
  names(forAdd) <- useTermVector

  ## use data.table because data.frame causes memory overflow
  dtmDt <- data.table(dtmDf)
  forAddDt <- data.table(forAdd)
  forPredictDtm <- cbind(dtmDt, forAddDt)

  ## Predict with model
  print("Start Prediction")
  predSVM <- predict(spamModel, forPredictDtm)

  modelResult <- data.frame(crawl_data_id=dCorpus$crawl_data_id, pred=predSVM)
  modelResult$user <- user
  print("End Prediction")
  print(summary(modelResult))

  print("DB Writing")
  sqlSave(conn, modelResult, tablename="trendtracker.t_svm_result_adhoc", rownames=FALSE, append=TRUE)
  
  rm(predSVM)
  rm(modelResult)
  rm(dtmDf)
  gc()
}

odbcClose(conn)

end.time <- Sys.time()
predictTime <- end.time - start.time

print("Prediction Time: ")
print(predictTime)
print("Prediction Complete")

rm(validationTm)
rm(tmValidationKeys)
#rm(tmValidationRoles)
#rm(tmValidationDocType)

#save.image(file="predict_svm_results_newSixPocket4.RData")
#save.image(file="predict_svm_results_forVal.RData")
