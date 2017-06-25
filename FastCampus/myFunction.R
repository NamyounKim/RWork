
makeDtm = function(filePathName, stopWord){
  library(readr)
  parsedData =read_csv(filePathName)
  
  colnames(parsedData) = c("id","pContent")

  library(tm)
  library(slam)
  library(dplyr)
  

  parsedDataRe = parsedData
  parsedDataRe$pContent = gsub(" ","  ",parsedDataRe$pContent)
  
  corp<-Corpus(DataframeSource(parsedDataRe))
  

  corp <- tm_map(corp, removePunctuation)
  

  corp <- tm_map(corp, removeNumbers)

  corp <- tm_map(corp, removeWords, stopWord)

  corp <- tm_map(corp, PlainTextDocument)
  

  dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(3,Inf)))

  colnames(dtm) = trimws(colnames(dtm))
  dtm = dtm[,nchar(colnames(dtm)) > 1]
  

  dtm <- removeSparseTerms(dtm, as.numeric(0.99))
  
  return(dtm)
}