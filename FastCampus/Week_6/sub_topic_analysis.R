library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

source("./tm_function.R") # sub_corpus로 DTM을 만드는 함수

#추출하고 싶은 토픽 번호
select_topic = c(10)

#선택한 토픽번호를 갖는 문서 추출
sub_parsedData = id_topic %>% filter(doc_topic %in% select_topic) %>% filter(maxProb > 0.5) %>% dplyr::select(text, doc_topic)

#------------------------------------------------------------------------------------
# sub_corpus로 DTM을 만드는 함수
dtm = makeDtm(parsedData = sub_parsedData$text, sr = 0.94, type = "tf-idf")
#------------------------------------------------------------------------------------

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtm_m = as.matrix(dtm)
cor_term = cor(dtm_m)

#Edge 개수 조절하기
cor_term[cor_term < 0.75] = 0

# 다른 노드와 연관성이 0인 노드 제거하기
removeTarget = colSums(cor_term) == 1
cor_term = cor_term[!removeTarget, !removeTarget]

# Network Map을 그리기 위한 객체 만들기 ------------------------------------------------------------------
net = network(cor_term, directed = FALSE)

# node 색상결정
# betweenness값 상위 20% 이면서 eigenvector 값이 상위 10%이면 "High" -> 빨강색
# betweenness값 상위 20% 이면서 eigenvector 값이 하위 90%이면 "Medium" -> 노란색
# betweenness값 하위 80% 이면 "Low" -> 회색
net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8)
                        ,ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium"), "Low")
node_color = c("Low" = "grey", "Medium" = "darkgoldenrod1", "High"="brown1")

# node 크기결정
dtm_tfidf_sum = colSums(dtm_m)
dtm_tfidf_sum = dtm_tfidf_sum[names(dtm_tfidf_sum) %in% rownames(cor_term)]
node_size = c("node.size" = round(dtm_tfidf_sum, digits = 1))

# Network edge size 값 설정하기 (단어간 상관계수 값 * 2)
set.edge.value(net, "edgeSize", cor_term)

# Network Map 화면에 그리기
ggnet2(net # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,color = "mode" # 노드 색상 구준 기준
       ,palette = node_color # 노드 색상
       ,size = node_size # 노드의 크기를 tf_idf의 값으로 다르게 하기
       ,edge.size = "edgeSize" # 엣지의 굵기를 위에서 계산한 단어간 상관계수에 따라 다르게 하기
       ,mode = "fruchtermanreingold"
       ,family = "AppleGothic"
       
)

#shiny에 입력할 수 있는 csv형태의 파일로 떨구기
write.table(sub_parsedData, "./LDA_output/sub_topic.csv", sep=",", row.names=FALSE)
