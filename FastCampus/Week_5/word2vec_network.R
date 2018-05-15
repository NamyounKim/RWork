library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

# 단어간 코사인 유사도 구하기
temp = cosineSimilarity(model, model)

# Edge 개수 조절하기 (0~1 사이 값으로 세팅)
temp[temp < 0.74] = 0

sparseRatio = colSums(temp == 0) / nrow(temp)

# sparse ratio 값 분포 확인
quantile(sparseRatio, seq(0,1,0.1))
temp = temp[sparseRatio<0.999, sparseRatio<0.999]

# Node 개수 조절하기 (0인 값 제외)
temp = temp[,colSums(temp)!=0]
temp = temp[rowSums(temp)!=0,]

# Network Map을 그리기 위한 객체 만들기
net = network(temp, directed = FALSE)

# Network의 betweenness값을 구하여 상위 10% 이상인 node에는 노란색 입혀주기
net %v% "mode" <- ifelse(betweenness(net) > quantile(betweenness(net), 0.9), "big", "small")
node_color = c("small" = "grey", "big" = "gold")

# Network edge size 값 설정하기 (단어간 상관계수 값 * 0.8)
set.edge.value(net, "edgeSize", temp * 0.8)

# Network Map 화면에 그리기
ggnet2(net # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,color = "mode" # 노드 색상 구준 기준
       ,palette = node_color # 노드 색상
       ,size = "degree" # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,edge.size = "edgeSize" # 엣지의 굵기를 위에서 계산한 단어간 상관계수에 따라 다르게 하기
       ,family = "AppleGothic"
       ) 

