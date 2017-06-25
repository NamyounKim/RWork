rm(list=ls())

start.time <- Sys.time()
source("/home/ruser/OperHunT/ML_functions.R")

#package check & install & load
libraryList <- c("dplyr","stringi","tm","caret","reshape")

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
#require(e1071)

############################################
## Read in TM results
############################################
#tm <- read.csv("../data/blog/Blog_TraingSet_case1_Uhtagger_Result.csv",
#               header=TRUE, fileEncoding="UTF-8")

#get parameters
arg <- commandArgs()

filePath <- arg[6] #C:\\Users\\78102\\workspace\\OperHunT
fileNamesConcat <- arg[7] #out_test_1.csv/out_test_2.csv

print(filePath)
print(fileNamesConcat)

setwd(filePath)
filePath <- paste(filePath,"/output",sep="")
fileNames <- strsplit(fileNamesConcat,"/")[[1]]

tm <- NULL

for(i in 1:length(fileNames)){

  fileName <- fileNames[i]
  tm <- rbind(tm, read.csv(paste(filePath,fileName,sep="/"), header=TRUE, fileEncoding="UTF-8"))
}

## TM results into document keywords matrices
tmKeyword <- fn_tm_keys(tm)
#tmRoles <- fn_tm_roles(tm)
tmDocType <- fn_tm_spam_yn(tm)

############################################
## Make DTM
############################################
dtm_df <- fn_makeDTM(tmKeyword)

dtm_df$crawl_data_id <- tmKeyword$crawl_data_id

## Append document type information
total <- merge(dtm_df, tmDocType, by="crawl_data_id")
#total <- merge(total, tmRoles, by="crawl_data_id")
noDocidTotal <- select(total, -(crawl_data_id))

############################################
## Train models
############################################

cvtrain <- trainControl(method="cv", number=5)
svmModel <- train(docType ~ ., data=noDocidTotal, method="svmLinear",
                   trControl=trainControl(method="cv", number=3),
                   tuneGrid=data.frame(.C=c(0.01)),
                   metric="Accuracy",
                   preProc=c("center", "scale"))             


end.time <- Sys.time()
svmtraintime <- end.time - start.time

print("Modeling Time:")
print(svmtraintime)
print("Modeling Complete")

rm(tm)
rm(tmKeyword)
#rm(tmRoles)
rm(tmDocType)
rm(dtm_df)
rm(total)
rm(start.time)
rm(end.time)

save.image(file="model_svm_final.RData")
