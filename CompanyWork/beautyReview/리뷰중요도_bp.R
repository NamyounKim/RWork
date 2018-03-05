#dyn.load("/usr/java/jdk1.8.0_151/jre/lib/amd64/server/libjvm.so")
.jcall("java/lang/System","S","getProperty","java.runtime.version")
library(NLP4kec)
library(readr)
library(data.table)
library(tm)
library(dplyr)
library(stringi)

load("./backup/forReview.RData")

parsed_strength = r_parser_r(raw_review_strength$content, language = "ko", korDicPath = "../dic/dictionary.txt")
parsed_weakness = r_parser_r(raw_review_weakness$content, language = "ko", korDicPath = "../dic/dictionary.txt")
parsed_etc = r_parser_r(raw_review_etc$content, language = "ko", korDicPath = "../dic/dictionary.txt")
parsed_all = r_parser_r(raw_review_all$content, language = "ko", korDicPath = "../dic/dictionary.txt")

#raw_review_all$content[352]
#r_parser_r(raw_review_all$content[352], language = "ko", korDicPath = "../dic/dictionary.txt")

stopWordDic = read_csv("../dic/stopword_ko.csv")
synonymDic = read_csv("../dic/synonym", trim_ws = F)

synonymProcess = function(targetParsedSet, sparseRatio){
  # 동의어 처리
  for (i in 1:nrow(synonymDic)){
    targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], targetParsedSet))
    for(j in 1:length(targetDocIdx)){
      docNum = targetDocIdx[j]
      targetParsedSet[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], targetParsedSet[docNum])
    }
  }
  return(targetParsedSet)
}

parsed_strength = synonymProcess(parsed_strength)
parsed_weakness = synonymProcess(parsed_weakness)
parsed_etc = synonymProcess(parsed_etc)
parsed_all = synonymProcess(parsed_all)

getDtmTfidf = function(targetParsedSet, sparseRatio){
  #Corpus 생성
  corp = VCorpus(VectorSource(targetParsedSet))
  
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
  dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf), 
                                              weighting = function(x) weightTfIdf(x, normalize = TRUE)))
  dtm = removeSparseTerms(dtm, as.numeric(sparseRatio))
  dtmMat = as.matrix(dtm)
  
  return(dtmMat)
}

dtmMat_strength = getDtmTfidf(parsed_strength, 0.9999)
dtmMat_weakness = getDtmTfidf(parsed_weakness, 0.9999)
dtmMat_etc = getDtmTfidf(parsed_etc, 0.9999)
dtmMat_all = getDtmTfidf(parsed_all, 0.9999)

# 문서길이 대비 단어 개수
makeDocScore = function(inputSet, inputDtmMat){
  tnDl = vector(length = nrow(inputSet))
  for(i in 1:nrow(inputSet)){
    #tnDl[i] = length(which(dtmMat[i,]>0)) * nchar(raw_review2[i,]$v_content) #사용단어수 / 문서길이(작을수록 스팸)
    tnDl[i] = sum(inputDtmMat[i,]) * nchar(inputSet[i,]$content) #
  }
  return(tnDl)
}

docScore_strength = makeDocScore(raw_review_strength, dtmMat_strength)
docScore_weakness = makeDocScore(raw_review_weakness, dtmMat_weakness)
docScore_etc = makeDocScore(raw_review_etc, dtmMat_etc)
docScore_all = makeDocScore(raw_review_all, dtmMat_all)

par(mfrow = c(1,4))
boxplot(docScore_strength, ylim=c(0,8000))
boxplot(docScore_weakness, ylim=c(0,8000))
boxplot(docScore_etc, ylim=c(0,8000))
boxplot(docScore_all, ylim=c(0,8000))

length(docScore_strength[docScore_strength>=1000])
length(docScore_strength)

length(docScore_weakness[docScore_weakness>=1000])
length(docScore_weakness)

length(docScore_all[docScore_all>=1000])/ length(docScore_all)
raw_review_all$docScore = docScore_all

#------------일단 여기 까지 ... -----------------
best_review = targetSetForPaser %>% filter(tnDl >= 1000) %>% group_by(v_productcd, v_reviewcd) %>% summarise(tnDl2 = sum(tnDl)) %>% mutate(rank2 = rank(-tnDl2))
best_review = merge(best_review, raw_review2 %>% select(v_reviewcd, v_reg_dtm), by = "v_reviewcd")
best_review = best_review %>% arrange(v_productcd, rank2)
best_review = best_review %>% filter(rank2 <= 5)
best_review = best_review[,c(2,1,5,3,4)]
colnames(best_review) = c("v_productcd","v_reviewcd","v_review_reg_dtm","score","ranking")

write.csv(best_review, "./best_review.csv", row.names = F)

#### 특정 상품 댓글 중요순으로 보기 #####
# 000025045 - 프리메라 오가니언스 워터
# SPR201708230000319498 - 헤라 루즈홀릭샤인
# P00003899 - 트리트먼트 라인 4종
# P00005040 - 설화수 퍼펙팅 쿠션
# SPR20150728000010322 - 해피바스 버블 핸드워시
# P00004032 - 일리윤 토탈에이징케어 클렌징 오일
# 000023888 - 바이탈뷰티 천삼액


p_review = raw_review2 %>% filter(v_productcd =="000025045") %>% arrange(-tnDl) %>% select(v_content,v_reg_dtm,tnDl)
p_review[1:10,]
write.csv(p_review[1:10,], "./p_review.csv", row.names = F)

raw_review2 %>% filter(v_reviewcd == "CRV20171021000619144")

temp = raw_review2 %>% group_by(v_productcd, bestYN) %>% summarise(n=n())
temp = dcast(temp, v_productcd ~ bestYN, sum, value.var="n" )
temp = merge(temp, raw_product2[,c(1,2)], by = "v_productcd", all.x = T)
temp = temp %>% mutate(review_n = N + Y) %>% mutate(bestRatio = Y*100/review_n)
temp = temp[,c(1,4,5,3,6)]
colnames(temp)[4] = "best_review_n"
temp = temp %>% filter(!is.na(v_productcd)) %>% arrange(-review_n)
write.csv(temp, "./상품별리뷰현황.csv", row.names = F)

write.csv(raw_review2, "./raw_review2.csv", row.names = F)
