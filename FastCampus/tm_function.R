
makeDtm <- function(parsedData, sr, type){
  #Corpus 생성
  corp = VCorpus(VectorSource(parsedData))
  
  #특수문자 제거
  corp = tm_map(corp, removePunctuation)
  
  #숫자 삭제
  corp = tm_map(corp, removeNumbers)
  
  #소문자로 변경
  corp = tm_map(corp, tolower)
  
  #특정 단어 삭제
  corp = tm_map(corp, removeWords, stopWordDic$stopword)
  
  #텍스트문서 형식으로 변환
  corp = tm_map(corp, PlainTextDocument)
  
  if(type == "tf"){
    dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))
  }else if(type == "tf-idf"){
    dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                                 weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기
  }else{
    print("타입 정보를 입력해 주세요!")
  }
  
  ## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
  colnames(dtm) = trimws(colnames(dtm))
  dtm = dtm[,nchar(colnames(dtm)) > 1]
  
  dtm = removeSparseTerms(dtm, as.numeric(sr))
  
  return(dtm)
}