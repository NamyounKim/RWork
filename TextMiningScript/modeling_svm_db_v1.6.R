rm(list=ls())
gc()

start.time <- Sys.time()
source("/home/ruser/TextPrism/RSource/ML_functions.R")
source("/home/ruser/TextPrism/RSource/config_modeling.R")

#package check & install & load
libraryList <- c("dplyr","stringi","tm","caret","reshape","RODBC","RODBCext","randomForest","doMC","pROC")

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
require(pROC)
#require(e1071)

registerDoMC(cores = coreNum)

############################################
## Read in TM results
############################################
#conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')
conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')

tm<-sqlQuery(conn,inputQuery)

odbcClose(conn)

############################################
## TM results Handling
############################################
#Change the blank to "#"
tm$keyword<-stri_replace_all_fixed(tm$keyword, " ","#")

#NS : 지역명
#NT : 브랜드명
#N : 명사
#Remove role
#tm <- tm[!tm$role %in% c('V','VZ','NB','NS','NT'),]

#Remain role
tm <- tm[tm$role %in% c('NB','NZ','NT','N'),]

###############################################
## TM results into document keywords matrices
###############################################
tmKeyword <- fn_tm_keys(tm)
print(paste("Traing DOC #: ",nrow(tmKeyword),sep=""))
#tmRoles <- fn_tm_roles(tm)
tmDocType <- fn_tm_target(tm)

############################################
## Make DTM
############################################
if(dtmMethod==1){
  dtm_df <- fn_makeDTM(tmKeyword, sparseRatio)
}else if(dtmMethod == 2){
  dtm_df <- fn_makeTfIdfDTM(tmKeyword, sparseRatio)
}else if(dtmMethod ==3){
  dtm_df <- fn_makeTfIdfDTM2(tmKeyword, sparseRatio, 0.1)
}else {
  stop("Wrong the dtmMethod Number!!")
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
#cvtrain <- trainControl(method="cv", number=7, classProbs = TRUE, summaryFunction = twoClassSummary)
#cvtrain <- trainControl(method="cv", number=7, classProbs = TRUE)
#grid <- data.frame(.C=c(0.0009, 0.001, 0.0025, 0.005))
#grid <- data.frame(.C=c(0.01))
#predModel <- train(target ~ ., data=noDocidTotal, method="svmLinear",	
#                  trControl=cvtrain,	
#                  tuneGrid=grid,
#                  metric="ROC",
#                  preProc=c("center", "scale"))	

##RandomForest##
cvtrain <- trainControl(method="cv", number=7, classProbs = TRUE)
#mtry = sqrt(p) where p is number of variables in x
predModel <- train(target ~ ., data=noDocidTotal, method="parRF",
                      trControl=cvtrain,
                      tuneGrid=data.frame(mtry=73),
                      metric="Accuracy",
                      preProc=c("center", "scale"))             

##RBF##
#cvtrain <- trainControl(method="cv", number=5)
#grid <- expand.grid(ntree=100, importance=TRUE, mtry=3)
#predModel <- train(target ~ ., data=noDocidTotal, method="parRF",
#                      trControl=cvtrain,
#                      tuneGrid=data.frame(mtry=5),
#                      metric="Accuracy",
#                      preProc=c("center", "scale"))  

print(predModel)

#importance variable list
impTerm <- varImp(predModel,scale = FALSE)
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
