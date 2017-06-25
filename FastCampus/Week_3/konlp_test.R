#KoNLP 설치
install.packages("KoNLP")
library(KoNLP)
library(stringi)

#KoNLP를 불러올때 에러가 난다면 JAVA의 경로를 다시 세팅
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_101")


#문석 대상 텍스트
sentence = "류현진(30·LA다저스)이 복귀전에서 5회를 버티지 못했다. 하지만 충분히 박수를 받을만한 경기였다."

#명사만 추출
extractNoun(sentence)

#세종사전 사용하기
useSejongDic()

#POS 태깅 정보 추출
termPos = SimplePos22(sentence)

#POS 태깅 정보를 활용한 명사, 동사 추출
txt = NULL

for(i in 1: length(termPos)){
  step1 = stri_split_fixed(termPos[i],"+")
  step2 = unlist(step1)
  for(j in 1:length(step2)){
    if(grepl("/NC",step2[j])){
      temp = unlist(stri_split_fixed(step2[j],"/"))
      temp = temp[1]
      txt = paste(txt, temp, sep = " uu")
    }else if(grepl("/PV",step2[j])){
      temp = unlist(stri_split_fixed(step2[j],"/"))
      temp = temp[1]
      txt = paste(txt, temp, collapse = " ")
    }
  }
}


#txtDf = read.csv("./HomeApplication_cafe.csv", stringsAsFactors = FALSE, header = TRUE, encoding = "bytes")
#txt = sapply(sentence, function(x) {paste(extractNoun(x), collapse=" ")}) 

