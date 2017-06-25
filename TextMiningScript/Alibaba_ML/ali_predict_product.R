rm(list=ls())
gc()

start.time <- Sys.time()

load(file="/home/ruser/TextPrism/modelResult/model_#product_7_0.999.RData")

source("/home/ruser/TextPrism/RSource/ML_functions.R")
source("/home/ruser/TextPrism/RSource/Alibaba/config_predict.R")

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

############################################
## Read in TM results
############################################
#conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')
conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')

tpResult <- sqlQuery(conn,inputQuery)

############################################
## TM results Handling
############################################
#Change the blank to "#"
tpResult$keyword<-stri_replace_all_fixed(tpResult$keyword, " ","#")

#NS : 지역명
#NT : 브랜드명
#N : 명사
#Remove role
#tpResult <- tpResult[!tpResult$role %in% c('V','VZ','NB','NS','NT'),]

#Remain role
tpResult <- tpResult[tpResult$role %in% c('NB','NZ','NT','N'),]

###############################################
## TM results into document keywords matrices
###############################################
corpus <- fn_tm_keys(tpResult)
#tmValidationRoles <- fn_tm_roles(validationTm)
#tmValidationDocType <- fn_tm_doc_type(validationTm)

print(paste("Target Document :",nrow(corpus)))

############################################
## Make DTM
############################################
dCorpus <- NULL
increment <- 1000
endRowNumCorpus <- nrow(corpus)

for(i in seq(from=1, to=nrow(corpus), by=increment)){
  
  print(paste(i," ~ ",(i+(increment-1)), sep=""))
  
  if((i+(increment-1)) <= nrow(corpus)){
    dCorpus <- corpus[i:(i+(increment-1)),]
  }
  else{
    dCorpus <- corpus[i:endRowNumCorpus,]
  }
  
  ############################################
  ## Make DTM
  ############################################
  if(dtmMethod==1){
    dtm_df <- fn_makeDTM(dCorpus, sparseRatio)
  }else if(dtmMethod == 2){
    dtm_df <- fn_makeTfIdfDTM(dCorpus, sparseRatio)
  }else if(dtmMethod ==3){
    dtm_df <- fn_makeTfIdfDTM2(dCorpus, sparseRatio, 0.1)
  }else {
    stop("Wrong the dtmMethod Number!!")
  }
  
  ## Remove superfluous var columns
  notUseTermVector <- setdiff(names(dtm_df), names(noDocidTotal))
  
  if(length(notUseTermVector) != 0){
    dtm_df <- dtm_df[, !(names(dtm_df) %in% notUseTermVector)]
    
    useTermVector <- setdiff(names(select(noDocidTotal, -target)), names(dtm_df))
    forAdd <- data.frame(matrix(0, ncol=length(useTermVector), nrow=nrow(dtm_df)))
    names(forAdd) <- useTermVector
    
    ## use data.table because data.frame causes memory overflow
    dtmDt <- data.table(dtm_df)
    forAddDt <- data.table(forAdd)
    forPredictDtm <- cbind(dtmDt, forAddDt)
  }else{
    forPredictDtm <- dtm_df
  }
  
  ## Predict with model
  print("Start Prediction")
  predResult <- predict(predModel, forPredictDtm, type = "prob")
  maxValue <- data.frame(apply(predResult,1,max))
  colnames(maxValue)<-'product_score'
  
  predResult$product <- colnames(predResult)[apply(predResult,1,which.max)]
  predResult$product_score <- maxValue$product_score
  predResultDf <- data.frame(crawl_data_id=dCorpus$crawl_data_id, pred=predResult)
  
  predResultDf <- subset(predResultDf, select = c(crawl_data_id, pred.product, pred.product_score))
  colnames(predResultDf) <- c('crawl_data_id','product_type','product_score')
 
  #modelResult$user <- user
  print("End Prediction")
  print(head(predResultDf))
  
  print("DB Writing")
  sqlSave(conn, predResultDf, tablename=insertTable, rownames=FALSE, append=TRUE)
  
  rm(predResult)
  rm(predResultDf)
  rm(dtm_df)
  gc()
}

odbcClose(conn)

end.time <- Sys.time()
predictTime <- end.time - start.time

print("Prediction Time: ")
print(predictTime)
print("Prediction Complete")

