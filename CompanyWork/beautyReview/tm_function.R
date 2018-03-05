library(tm)

makeDtm <- function(parsedText, sr, dtmType){
  
  parsedText = as.vector(parsedText$parsed)
  #Corpus 생성
  corp = VCorpus(VectorSource(parsedText))
  
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
  #Document Term Matrix 생성 (단어 Length는 2로 세팅)
  if(dtmType == "tf"){
    dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
    dtm = removeSparseTerms(dtm, as.numeric(sr))
    
    return(dtm)
  }else if(dtmType == "tfidf"){
    dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                                 weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기
    dtmW = removeSparseTerms(dtmW, as.numeric(sr))
    return(dtmW)
  }else if(dtmType == "tdm-tf"){
    tdm = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf))) #Tf-Idf 가중치 주기))
    tdm = removeSparseTerms(tdm, as.numeric(sr))
  }else if(dtmType == "tdm-tfidf"){
    tdm = TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf),
                                                weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기))
    tdm = removeSparseTerms(tdm, as.numeric(sr))
  }
}


getAttributeScore = function(cosSimMat, tdmMat, attKeyword, cutOff){
  
  #코사인 유사도 매트릭스에서 토픽 키워드만 남기기
  funcSim = cosSimMat[rownames(cosSimMat) %in% attKeyword,]
  
  #funcSim과 tdmMat의 term 맞추기
  sub_tdmMat = tdmMat[rownames(tdmMat) %in% colnames(funcSim),]
  funcSim = funcSim[,colnames(funcSim) %in% rownames(sub_tdmMat)]
  funcSim = funcSim[,order(colnames(funcSim))]
  
  #funcSim의 값 조정하기
  funcSim[funcSim < cutOff] = 0
  
  #문서 번호 부여하기
  colnames(sub_tdmMat) = seq(1:ncol(sub_tdmMat))
  
  # CosSim X TDM 내적 구하기
  docScore = funcSim %*% sub_tdmMat
  
  # Transpose
  docScore = t(docScore)
  docScore = as.data.frame(docScore)
  
  output_list = list(docScore = docScore, funcSim=  funcSim, sub_tdmMat = sub_tdmMat)
  
  return(output_list)
}