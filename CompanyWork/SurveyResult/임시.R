#install.packages("readxl")
library(readxl)
library(readr)
library(dplyr)
library(NLP4kec)
library(stringi)

library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

raw_data = read_xlsx("./RawData_suvey.xlsx", sheet = 1, col_names = F)

stopWordDic = read_csv("../dic/stopword_ko.csv")
synonymDic = read_csv("../dic/synonym", trim_ws = F)

raw_data2 = raw_data %>% select(X__21, X__22, X__29)
raw_data2 = raw_data2[2:nrow(raw_data2),]
colnames(raw_data2) = c("referSite", "reviewExp", "opinion")
raw_data2$id = rownames(raw_data2)

raw_referSite = raw_data2 %>% filter(!is.na(referSite)) %>% select(id, referSite)
raw_reviewExp = raw_data2 %>% filter(!is.na(reviewExp)) %>% select(id, reviewExp)
raw_opinion = raw_data2 %>% filter(!is.na(opinion)) %>% select(id, opinion)


parsed_referSite = r_parser_r(raw_referSite$referSite, language = "ko", korDicPath = "../dic/dictionary.txt")
parsed_reviewExp = r_parser_r(raw_reviewExp$reviewExp, language = "ko", korDicPath = "../dic/dictionary.txt")
parsed_opinion = r_parser_r(raw_opinion$opinion, language = "ko", korDicPath = "../dic/dictionary.txt")

#동의어 처리
synonymProcess = function(targetParsedSet){
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

parsed_referSite = synonymProcess(parsed_referSite)
parsed_reviewExp = synonymProcess(parsed_reviewExp)

## 단어간 스페이스 하나 더 추가하기 ##
parsed_referSite = gsub(" ","  ",parsed_referSite)
parsed_reviewExp = gsub(" ","  ",parsed_reviewExp)

dtm = makeDtm(parsed_referSite, sr = 0.9999, dtmType = "tf")
dtmMat = as.matrix(dtm)

referRanking = sort(colSums(dtmMat), decreasing = T)[1:100]

## 리뷰 경험 부분 ------------------------------------------------------------------------------------
dtm2 = makeDtm(parsed_reviewExp, sr = 0.999, dtmType = "tf")
dtmMat2 = as.matrix(dtm2)

sort(colSums(dtmMat2), decreasing = T)



#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
cor_termW = cor(dtmMat2)

#Edge 개수 조절하기
cor_termW[cor_termW < 0.2] = 0

# 다른 노드와 연관성이 0인 노드 제거하기
removeTarget = colSums(cor_termW) == 1
cor_termW = cor_termW[!removeTarget, !removeTarget]

# Network Map을 그리기 위한 객체 만들기
net = network(cor_termW, directed = FALSE)

# betweenness값 상위 20% 이면서 eigenvector 값이 상위 10%이면 "High" -> 빨강색
# betweenness값 상위 20% 이면서 eigenvector 값이 하위 90%이면 "Medium" -> 노란색
# betweenness값 하위 80% 이면 "Low" -> 회색
net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8)
                        ,ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium"), "Low")
node_color = c("Low" = "grey", "Medium" = "darkgoldenrod1", "High"="brown1")

# Network edge size 값 설정하기 (단어간 상관계수 값 * 2)
set.edge.value(net, "edgeSize", cor_termW * 2)

# Network Map 화면에 그리기
ggnet2(net # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,color = "mode" # 노드 색상 구준 기준
       ,palette = node_color # 노드 색상
       ,size = "degree" # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,edge.size = "edgeSize" # 엣지의 굵기를 위에서 계산한 단어간 상관계수에 따라 다르게 하기
       ,mode = "fruchtermanreingold"
       ,family = "AppleGothic"
       ,layout.par = list(cell.pointcellrad=100000) # 네트워크 맵 레이아웃 조정하기
)

## 리뷰 경험 부분 ------------------------------------------------------------------------------------
dtm3 = makeDtm(parsed_opinion, sr = 0.999, dtmType = "tf")
dtmMat3 = as.matrix(dtm3)

sort(colSums(dtmMat3), decreasing = T)



#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
cor_termW = cor(dtmMat3)

#Edge 개수 조절하기
cor_termW[cor_termW < 0.2] = 0

# 다른 노드와 연관성이 0인 노드 제거하기
removeTarget = colSums(cor_termW) == 1
cor_termW = cor_termW[!removeTarget, !removeTarget]

# Network Map을 그리기 위한 객체 만들기
net = network(cor_termW, directed = FALSE)

# betweenness값 상위 20% 이면서 eigenvector 값이 상위 10%이면 "High" -> 빨강색
# betweenness값 상위 20% 이면서 eigenvector 값이 하위 90%이면 "Medium" -> 노란색
# betweenness값 하위 80% 이면 "Low" -> 회색
net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8)
                        ,ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium"), "Low")
node_color = c("Low" = "grey", "Medium" = "darkgoldenrod1", "High"="brown1")

# Network edge size 값 설정하기 (단어간 상관계수 값 * 2)
set.edge.value(net, "edgeSize", cor_termW * 2)

# Network Map 화면에 그리기
ggnet2(net # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,color = "mode" # 노드 색상 구준 기준
       ,palette = node_color # 노드 색상
       ,size = "degree" # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,edge.size = "edgeSize" # 엣지의 굵기를 위에서 계산한 단어간 상관계수에 따라 다르게 하기
       ,mode = "fruchtermanreingold"
       ,family = "AppleGothic"
       ,layout.par = list(cell.pointcellrad=100000) # 네트워크 맵 레이아웃 조정하기
)
