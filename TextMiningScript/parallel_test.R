library(foreach)
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

start.time <- Sys.time()

foreach(i=1:nrow(corpus))%dopar%{

  ## Make DTM
  dtmDf <- fn_makeDTM(corpus[i,],0.999)
  
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
  
  modelResult <- data.frame(crawl_data_id=corpus[i,]$crawl_data_id, pred=predSVM)
  modelResult$user <- user
  print("End Prediction")
  print(modelResult)
  
  #print("DB Writing")
  #sqlSave(conn, modelResult, tablename="trendtracker.t_svm_result_adhoc", rownames=FALSE, append=TRUE)
  
  rm(predSVM)
  rm(modelResult)
  rm(dtmDf)
  gc()
}

end.time <- Sys.time()
predictTime <- end.time - start.time

print("Prediction Time: ")
print(predictTime)
print("Prediction Complete")
