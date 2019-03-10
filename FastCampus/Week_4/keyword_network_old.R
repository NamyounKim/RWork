library(tm)

#아래 코드를 수행하기 이전에 반드시 DTM을 생성해야 합니다.
#text_handling.R 소스코드 참고 하세요.
corp = readRDS("./raw_data/corpus_petition.RDS")
dtm = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf)))
dtm = removeSparseTerms(dtm, as.numeric(0.98))

# 1. 연관 키워드 추출 및 TF-IDF 가중치 ------------------------------------------------------------------------------------------------------------------------------------
# "청원"의 연관 키워드 구하기
findAssocs(dtm, terms = "청원", corlimit = 0.1)

# 직접 단어간 상관관계 구하기
dtm_m = as.matrix(dtm)
cor_term = cor(dtm_m)
cor_ref = cor_term[,"청원"]
cor_ref

#TF-IDF 값으로 연관 키워드 추출하기
dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                            weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtmW) = trimws(colnames(dtmW))
dtmW = dtmW[,nchar(colnames(dtmW)) > 1]

dtmW = removeSparseTerms(dtmW, as.numeric(0.98))

findAssocs(dtmW, "청원", 0.05)


# 2. 키워드 네트워크 분석 및 시각화 -------------------------------------------------------------------------------------------------------------------------------------
install.packages(c("igraph", "network", "sna", "GGally")) #패키지 한꺼번에 설치하기
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtmW_m = as.matrix(dtmW)
cor_termW = cor(dtmW_m) # 상관계수 매트릭스 초기화

#Edge 개수 조절하기
cor_termW[cor_termW < 0.05] = 0

# 다른 노드와 연관성이 0인 노드 제거하기
removeTarget = colSums(cor_termW) == 1
cor_termW = cor_termW[!removeTarget, !removeTarget]

# Network Map을 그리기 위한 객체 만들기
net = network(cor_termW, directed = FALSE)

# 노드 색상 결정
# betweenness값 상위 20% 이면서 eigenvector 값이 상위 10%이면 "High" -> 빨강색
# betweenness값 상위 20% 이면서 eigenvector 값이 하위 90%이면 "Medium" -> 노란색
# betweenness값 하위 80% 이면 "Low" -> 회색
net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8)
                        ,ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium")
                        , "Low")
node_color = c("Low" = "grey", "Medium" = "darkgoldenrod1", "High"="brown1")

# node 크기결정 (tf-idf 값으로 사용)
dtm_tfidf_sum = colSums(dtmW_m)
dtm_tfidf_sum = dtm_tfidf_sum[names(dtm_tfidf_sum) %in% rownames(cor_termW)]
node_size = c("node.size" = round(dtm_tfidf_sum, digits = 0))

# Network edge size 값 설정하기 (단어간 상관계수 값 * 2)
set.edge.value(net, "edgeSize", cor_termW * 2)

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
       #,layout.par = list(cell.pointcellrad=100000) # 네트워크 맵 레이아웃 조정하기
       #,legend.size = 3
       )
#"circle"
#"kamadakawai"
#"fruchtermanreingold"
#circrand

word_network = data.frame(word = rownames(cor_termW),
                          centrality = degree(net), #연결 중심성 구하기 
                          betweenness = betweenness(net), #매개 중심성 구하기
                          closeness = closeness(net, cmode="suminvundir"), # 근접 중심성 구하기
                          eigenvector = evcent(net) # 고유벡터 중심성 구하기
                          )
word_network %>% arrange(-betweenness)

# 3. 특정 키워드만 선택한 네트워크 맵 -------------------------------------------------------------------------------------------------------------------------------------
#keyword = c("박수진","특혜","삼성병원")
keyword = c("대한항공","갑질","국민")

cor_termW = cor(dtmW_m)
cor_termW[cor_termW < 0.05] = 0

sub_cor_term = cor_termW[,keyword]
sub_cor_term = sub_cor_term[!(rownames(sub_cor_term) %in% keyword),]
sub_cor_term = sub_cor_term[rowSums(sub_cor_term)>0,]

net2 = network(sub_cor_term, directed = FALSE, matrix.type="bipartite")

ggnet2(net2 # 네트워크 객체
       ,label=TRUE # 노드에 라벨 표현 여부
       ,label.size = 3 # 라벨 폰트 사이즈
       ,edge.size = sub_cor_term[sub_cor_term>0] * 2
       ,size = degree(net2) # 노드의 크기를 degree cetrality값에 따라 다르게 하기
       ,family = "AppleGothic"
       )
