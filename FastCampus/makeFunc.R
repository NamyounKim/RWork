makeDtm = function(parsedData, stopWord, removeRatio){
  
  #Corpus 생성
  corp = VCorpus(VectorSource(parsedData))
  
  #특수문자 제거
  corp = tm_map(corp, removePunctuation)
  
  #특정 단어 삭제
  corp = tm_map(corp, removeWords, stopWord)
  
  #동의어 처리
  for (j in seq(corp))
  {
    corp[[j]] <- gsub("미장센", "미쟝센", corp[[j]])
    corp[[j]] <- gsub("미쟝셴", "미쟝센", corp[[j]])
    corp[[j]] <- gsub("미쟝셴", "미쟝센", corp[[j]])
  }
  ##################################################################
  
  #텍스트문서 형식으로 변환
  corp = tm_map(corp, PlainTextDocument)
  
  #Document Term Matrix 생성 (단어 Length는 2로 세팅)
  dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))
  
  ## 한글자 단어 제외하기 ##
  colnames(dtm) = trimws(colnames(dtm))
  dtm = dtm[,nchar(colnames(dtm)) > 1]
  
  #Sparse Terms 삭제
  dtm = removeSparseTerms(dtm, as.numeric(removeRatio))
  
  return(dtm)
}