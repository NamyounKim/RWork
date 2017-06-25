rm(list=ls())
gc()

start.time <- Sys.time()
source("/home/ruser/TextPrism/RSource/ML_functions.R")

#package check & install & load
libraryList <- c("dplyr","stringi","tm","caret","reshape","RODBC","RODBCext","randomForest","doMC")

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
require(reshape)
require(RODBC)
require(RODBCext)
require(randomForest)
require(doMC)
#require(e1071)

##### Option #####
tfIdf <- FALSE
sparseRatio <- 0.997
modelNumber <- "test"
registerDoMC(cores = 5)

############################################
## Read in TM results
############################################
conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')

tm<-sqlQuery(conn,'SELECT crawl_data_id, crawled_date, keyword, ranking as count, role, spam_yn FROM trendtracker.t_tp_result_rank_train where user="ml_train2" and role <> "$$";')

odbcClose(conn)

## TM results into document keywords matrices
tmKeyword <- fn_tm_keys(tm)
print(paste("Traing DOC #: ",nrow(tmKeyword),sep=""))
#tmRoles <- fn_tm_roles(tm)
tmDocType <- fn_tm_spam_yn(tm)

############################################
## Make DTM
############################################
if(tfIdf==TRUE){
  dtm_df <- fn_makeTfIdfDTM(tmKeyword, sparseRatio)
}else{
  dtm_df <- fn_makeDTM(tmKeyword, sparseRatio)
}

dtm_df$crawl_data_id <- tmKeyword$crawl_data_id

## Append document type information
total <- merge(dtm_df, tmDocType, by="crawl_data_id")
#total <- merge(total, tmRoles, by="crawl_data_id")
noDocidTotal <- subset(total, select=(-crawl_data_id))

############################################
## Train models
############################################
print(paste("Creating Model  #", modelNumber))
##SVM##
cvtrain <- trainControl(method="cv", number=5, classProbs = TRUE)
grid <- data.frame(.C=c(0.001))
spamModel <- train(spam_yn ~ ., data=noDocidTotal, method="svmLinear",	
                  trControl=cvtrain,	
                  tuneGrid=grid,	
                  metric="Accuracy",	
                  preProc=c("center", "scale"))	

##RandomForest##
#cvtrain <- trainControl(method="cv", number=5)
#grid <- expand.grid(ntree=100, importance=TRUE, mtry=3)
#spamModel <- train(spam_yn ~ ., data=noDocidTotal, method="parRF",
#                      trControl=cvtrain,
#                      tuneGrid=data.frame(mtry=5),
#                      metric="Accuracy",
#                      preProc=c("center", "scale"))             

print(spamModel)
end.time <- Sys.time()
svmtraintime <- end.time - start.time

print("Modeling Time:")
print(svmtraintime)
print("Modeling Complete")

rm(tm)
#rm(tmKeyword)
#rm(tmRoles)
rm(tmDocType)
rm(dtm_df)
rm(total)
rm(start.time)
rm(end.time)

print(paste("Created the model.  #", modelNumber, "_", sparseRatio,sep=""))
save.image(file=paste("/home/ruser/TextPrism/modelResult/model_#",modelNumber,"_", sparseRatio,".RData",sep=""))
