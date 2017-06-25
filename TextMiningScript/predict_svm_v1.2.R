rm(list=ls())

libraryList <- c("dplyr","stringi","tm","caret","data.table","reshape")

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
#require(e1071)

start.time <- Sys.time()

## Load model data
load(file="model_svm_final.RData")
source("/home/ruser/OperHunT/ML_functions.R")

#get parameters
arg <- commandArgs()

filePath <- arg[6] #C:\\Users\\78102\\workspace\\OperHunT
fileNamesConcat <- arg[7] #out_test_1.csv/out_test_2.csv

print(filePath)
print(fileNamesConcat)

setwd(filePath)
filePath <- paste(filePath,"/output",sep="")
fileNames <- strsplit(fileNamesConcat,"/")[[1]]


############################################
## Read in TM results
############################################
#load(file="tm_result.RData")
validationTm <- NULL
for(i in 1:length(fileNames)){

  fileName <- fileNames[i]
  validationTm <- rbind(validationTm, read.csv(paste(filePath,fileName,sep="/"), header=TRUE, fileEncoding="UTF-8"))
}


tmValidationKeys <- fn_tm_keys(validationTm)
#tmValidationRoles <- fn_tm_roles(validationTm)
#tmValidationDocType <- fn_tm_doc_type(validationTm)

## Make DTM
dtmDf <- fn_makeDTM(tmValidationKeys)

## Remove superfluous var columns
notUseTermVector <- setdiff(names(dtmDf), names(noDocidTotal))
dtmDf <- dtmDf[, !(names(dtmDf) %in% notUseTermVector)]

useTermVector <- setdiff(names(select(noDocidTotal, -docType)), names(dtmDf))
forAdd <- data.frame(matrix(0, ncol=length(useTermVector), nrow=nrow(dtmDf)))
names(forAdd) <- useTermVector

## use data.table because data.frame causes memory overflow
dtmDt <- data.table(dtmDf)
forAddDt <- data.table(forAdd)
forPredictDtm <- cbind(dtmDt, forAddDt)

## Predict with model
#predSVM <- predict(svmModel, forPredictDtm)
predSVM <- predict(svmModel, forPredictDtm)

print("predSVM : ")
print(predSVM)
modelResult <- data.frame(crawl_data_id=tmValidationKeys$crawl_data_id, pred=predSVM)
print("modelResult : ")
print(modelResult)


end.time <- Sys.time()
predicttime <- end.time - start.time

print("Prediction Time:")
print(predicttime)
print("Prediction Complete")

rm(validationTm)
rm(tmValidationKeys)
#rm(tmValidationRoles)
#rm(tmValidationDocType)

save.image(file="predict_svm_results.RData")
