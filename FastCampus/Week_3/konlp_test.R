#KoNLP 설치
install.packages("KoNLP")
library(KoNLP)
library(stringi)

#KoNLP를 불러올때 에러가 난다면 JAVA의 경로를 다시 세팅
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_101")

#문석 대상 텍스트
sentence1 = "류현진(30·LA다저스)이 복귀전에서 5회를 버티지 못했다. 하지만 충분히 박수를 받을만한 경기였다."
sentence2 = "샌프란시스코 자이언츠 내야수 황재균은 2일(한국시간) PNC파크에서 열린 피츠버그 파이어리츠와의 원정 시리즈 두번째 경기에서 8회초 대타로 등장, 상대 투수 후안 니카시오를 상대로 좌익수 방면 2루타를 때렸다."

#명사만 추출
extractNoun(sentence1)
extractNoun(sentence2)

# 세종사전 사용하기
useSejongDic()

# NIA(한국정보화진흥원) 형태소사전 사용하기
useNIADic(category_dic_nms = "sports")

#POS 태깅 정보 추출
SimplePos22(sentence)

#################################################
# NLP4kec와 비교
library(NLP4kec)
test = text_parser(path = "./test_text.csv", language = "ko", korDicPath = "./dictionary.txt")
test
