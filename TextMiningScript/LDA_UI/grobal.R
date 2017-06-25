rm(list=ls())
gc()

start.time <- Sys.time()
source("/home/ruser/TextPrism/RSource/ML_functions.R")
source("/home/ruser/TextPrism/RSource/createNamJson.R")

#package check & install & load
libraryList <- c("dplyr","stringi","tm","reshape","RODBC","RODBCext","topicmodels","servr","LDAvis")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
require(dplyr)
require(stringi)
require(tm)
require(reshape)
require(slam)
require(SnowballC)
require(topicmodels)
require(RODBC)
require(RODBCext)
require(servr)
require(LDAvis)

ldaAnalysis <- function(inputQuery, sparse, name, k, numTermByTopic, MSC){
  sparseRe <- gsub("0.","_",sparse)

  
  ############################################
  ## Read in TM results
  ############################################
  
  #DB Connection
  print("Connect DB")
  #conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')
  conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')
  print("Loading Data from DB")
  
  tm<-sqlQuery(conn,inputQuery)
  odbcClose(conn)
  
  ## TM results into document keywords matrices
  print("Make DTM")
  tmKeyword <- fn_tm_keys(tm)
  print(paste("Total Document :",nrow(tmKeyword)))
  
  ####################################
  ## Manual Spam Check
  ####################################
  if(MSC){
    spamDocId <- read.table(file="spamDocId.txt", header=TRUE)
    spamCheck <- tmKeyword$crawl_data_id %in% spamDocId$spamDocId
    tmKeyword <- tmKeyword[!spamCheck,]
  }
  
  ##Duplication Check
  dupCheck <- duplicated(tmKeyword[,2])
  tmKeyword <- tmKeyword[!dupCheck,]
  
  print(paste("Target Document :",nrow(tmKeyword)))
  corp<-Corpus(DataframeSource(tmKeyword))
  dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
  dtm <- removeSparseTerms(dtm, as.numeric(sparse))
  
  ##Remove low tf-idf col and row
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  new_dtm <-dtm[,term_tfidf >= 0.1]
  new_dtm <-new_dtm[row_sums(new_dtm)>0,]
  
  ############################################
  ## Running LDA
  ############################################
  print("Start LDA")
  SEED <-2010
  
  lda_tm <- LDA(new_dtm, control=list(seed=SEED), k=as.numeric(k))
  
  doc_topic <- topics(lda_tm,1)
  term_topic <- terms(lda_tm, numTermByTopic)
  
  
  write.table(term_topic, paste("/home/ruser/TextPrism/output/",name,sparseRe,"_",k,"_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)
  
  
  #Doc_topic result making
  doc_topic_df <- as.data.frame(doc_topic)
  doc_topic_df$rown <- as.numeric(row.names(doc_topic_df))
  tmKeyword$rown <- as.numeric(row.names(tmKeyword))
  
  #doc prob make
  docProb <- posterior(lda_tm)$topics %>% as.matrix
  docProb_df <- as.data.frame(docProb)
  docProb_df$rown <- as.numeric(row.names(docProb_df))
  max<-NULL
  for(i in 1:nrow(docProb_df)){
    max<-rbind(max,max(docProb[i,]))
  }
  docProb_df$maxProb<-max
  
  id_topic <- merge(doc_topic_df, docProb_df, by="rown")
  id_topic <- merge(id_topic, tmKeyword, by="rown")
  id_topic <- subset(id_topic,select=c("rown","doc_topic","crawl_data_id","maxProb"))
  
  write.table(id_topic, paste("/home/ruser/TextPrism/output/",name,sparseRe,"_",k,"_raw","_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)
  
  outPutForQV <- fn_LDA_Result_for_QV(term_topic)
  write.table(outPutForQV, paste("/home/ruser/TextPrism/output/",name,sparseRe,"_",k,"_QV","_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)
  
}