library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

raw_review_weakness$parsedContent = parsed_weakness
raw_review_strength$parsedContent = parsed_strength

inputData = raw_review_strength %>% filter(brand_nm == "이니스프리")

#write_csv(inputData, "./input.csv", col_names = T)

dtmMat_weakness = getDtmTfidf(inputData$parsedContent, 0.998)
wordAgg = colSums(dtmMat_weakness)
wordAgg = as.data.frame(wordAgg)
wordAgg$word = rownames(wordAgg)

dim(dtmMat_weakness)
cor_termW = cor(dtmMat_weakness)

#Edge 개수 조절하기
cor_termW[cor_termW < 0.5] = 0

# 다른 노드와 연관성이 0인 노드 제거하기
removeTarget = colSums(cor_termW) == 1
cor_termW = cor_termW[!removeTarget, !removeTarget]

# Network Map을 그리기 위한 객체 만들기
net = network(cor_termW, directed = FALSE)

# Network의 betweenness값을 구하여 상위 10% 이상인 node에는 노란색 입혀주기
net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8), ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium"), "Low")
node_color = c("Low" = "grey", "Medium" = "darkgoldenrod1", "High"="brown1")

# Network edge size 값 설정하기 (단어간 상관계수 값 * 2)
set.edge.value(net, "edgeSize", cor_termW)

# Network Map 화면에 그리기
ggnet2(net # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,color = "mode" # 노드 색상 구준 기준
       ,palette = node_color # 노드 색상
       ,size = "degree" # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,edge.size = "edgeSize" # 엣지의 굵기를 위에서 계산한 단어간 상관계수에 따라 다르게 하기
       ,mode = "fruchtermanreingold"
       #,mode = "kamadakawai"
)

word_network = data.frame(word = rownames(cor_termW),
                          centrality = degree(net), #연결 중심성 구하기 
                          betweenness = betweenness(net), #매개 중심성 구하기
                          
                          closeness = closeness(net), # 근접 중심성 구하기
                          eigenvector = evcent(net) # 고유벡터 중심성 구하기
)

word_network = merge(wordAgg, word_network, by="word", all.x = T)

write.csv(wordAgg, "./wordAgg", row.names = T)

# 특정 키워드 연관키워드
centerKeyword = c("모이스처","흡수","아쉬움")

cor_termW = cor(as.matrix(dtmMat_weakness))

#Edge 개수 조절하기
cor_termW[cor_termW < 0.05] = 0

sub_cor_term = cor_termW[,centerKeyword]
sub_cor_term = sub_cor_term[!(rownames(sub_cor_term) %in% centerKeyword),]
sub_cor_term = sub_cor_term[rowSums(sub_cor_term) > 0, ]

net2 = network(sub_cor_term, directed = F, matrix.type="bipartite")

ggnet2(net2 # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,size = degree(net2) # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,edge.size = sub_cor_term[sub_cor_term>0]*3
       )

# 원문보기
head(inputData)
inputData$content[grepl("흡수",inputData$parsedContent)]
