#======================================
# 연관키워드 네트워크 맵 그리기
#======================================
#install.packages(c("igraph", "network", "sna", "GGally")) #패키지 한꺼번에 설치하기
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
#dtmW_m = as.matrix(dtmW2)
#cor_termW = cor(dtmW_m)

skin_dtmW = target_tfidf %>% filter(brand_nm =="헤라") %>% select(34:ncol(target_tfidf))
skin_dtmW = skin_dtmW %>% select(which(colSums(skin_dtmW) !=0.0))
cor_termW = cor(skin_dtmW)

#Edge 개수 조절하기
cor_termW_copy = cor_termW
cor_termW_copy[cor_termW_copy < 0.07] = 0

# Network Map을 그리기 위한 객체 만들기
net = network(cor_termW_copy, directed = FALSE)

# Network의 betweenness값을 구하여 상위 10% 이상인 node에는 노란색 입혀주기
net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8), ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium"), "Low")
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
)
#"circle"
#"kamadakawai"
#"fruchtermanreingold"
#circrand

word_network = data.frame(word = rownames(cor_termW),
                          centrality = degree(net), #연결 중심성 구하기 
                          betweenness = betweenness(net), #매개 중심성 구하기
                          
                          closeness = closeness(net), # 근접 중심성 구하기
                          eigenvector = evcent(net) # 고유벡터 중심성 구하기
)

## 특정 키워드만 선택한 네트워크 맵 그리기 ##
keyword = c("정수기","혜택","드럼")
sub_cor_term = cor_termW[,keyword]
sub_cor_term = sub_cor_term[!(rownames(sub_cor_term) %in% keyword),]
sub_cor_term = sub_cor_term[rowSums(sub_cor_term)>0,]

net2 = network(sub_cor_term, directed = FALSE, matrix.type="bipartite")

ggnet2(net2 # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,edge.size = sub_cor_term[sub_cor_term>0] * 2
       ,size = degree(net2) # 노드의 크기를 degree cetrality값에 따라 다르게 하기
)
