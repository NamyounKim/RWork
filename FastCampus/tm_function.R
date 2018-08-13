
makeDtm <- function(parsedData, sr, type){
  #Corpus 
  corp = VCorpus(VectorSource(parsedData))
  

  corp = tm_map(corp, removePunctuation)
  

  corp = tm_map(corp, removeNumbers)
  

  corp = tm_map(corp, tolower)
  

  corp = tm_map(corp, removeWords, stopWordDic$stopword)
  

  corp = tm_map(corp, PlainTextDocument)
  
  if(type == "tf"){
    dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))
  }else if(type == "tf-idf"){
    dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                                 weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기
  }else{
    print("Input DTM type!")
  }
  
  colnames(dtm) = trimws(colnames(dtm))
  dtm = dtm[,nchar(colnames(dtm)) > 1]
  
  dtm = removeSparseTerms(dtm, as.numeric(sr))
  
  return(dtm)
}