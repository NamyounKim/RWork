makeDtm = function(inputData, sparseRatio, tfIdf){
  
  #Corpus 생성
  corp = VCorpus(VectorSource(inputData))
  
  #특수문자 제거
  corp = tm_map(corp, removePunctuation)
  
  #숫자 삭제
  corp = tm_map(corp, removeNumbers)
  
  #소문자로 변경
  corp = tm_map(corp, tolower)
  
  #특정 단어 삭제
  corp = tm_map(corp, removeWords, stopWordDic$stopword)
  
  ##################################################################
  
  #텍스트문서 형식으로 변환
  corp = tm_map(corp, PlainTextDocument)
  
  if(tfIdf == TRUE){
    dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                                 weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기
    
  }else{
    dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
  }
  
  ## 한글자 단어 제외하기 ##
  colnames(dtm) = trimws(colnames(dtm))
  dtm = dtm[,nchar(colnames(dtm)) > 1]
  
  #Sparse Terms 삭제
  dtm = removeSparseTerms(dtm, as.numeric(sparseRatio))
  
  return(dtm)
}