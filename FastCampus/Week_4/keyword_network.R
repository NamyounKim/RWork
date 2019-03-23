library(tm)
library(dplyr)
library(tidyr)
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)
library(ggraph)
library(data.table)
library(stringi)
library(readr)

#아래 코드를 수행하기 이전에 반드시 DTM을 생성해야 합니다.
#text_handling.R 소스코드 참고 하세요.
corp = readRDS("./raw_data/corpus_petition.RDS")

#TF-IDF 값으로 연관 키워드 추출하기
dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                             weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기


## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtmW) = trimws(colnames(dtmW))
dtmW = dtmW[,nchar(colnames(dtmW)) > 1]

# Remove sparse term
dtmW = removeSparseTerms(dtmW, as.numeric(0.98))

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtmW_m = as.matrix(dtmW)
cor_termW = cor(dtmW_m) # 상관계수 매트릭스 초기화

#Edge 개수 조절하기
cor_termW[cor_termW < 0.05] = 0

# 동시출현 단어 상관계수 데이터셋 만들기
co_occurence_df = data.table(
  word1 = character(),
  word2 = character(),
  cor_value = numeric()
)
for(i in 1:length(rownames(cor_termW))){
  t = i+1
  if(i == length(rownames(cor_termW))){
    break()
  }
  for(j in t:length(colnames(cor_termW))){
    if(cor_termW[i,j] > 0 & cor_termW[i,j] < 1){
      add_row = list(rownames(cor_termW)[i], colnames(cor_termW)[j], cor_termW[i,j])
      co_occurence_df = rbind(co_occurence_df, add_row)
    }
  }
}

# 나중에 shiny 대시보드 사용을 위해 저장
saveRDS(co_occurence_df,"./Week_4/shiny_code_2/co_occurence_df.rds")

# 스타일 설정
set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")

# 특정 단어 중심의 네트워크 -------------------------------------------------------------------------------------------------------------------
one_word = "임금" # 중심 단어 세팅
one_word_graph = co_occurence_df %>%
  filter(word1 == one_word | word2 == one_word) %>% 
  top_n(20) %>% # 보여질 연관단어 개수
  graph_from_data_frame(directed = F)# 그래프 형태의 데이터셋으로 변환

ggraph(graph = one_word_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", end_cap = circle(.1, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()


# 전체 네트워크 맵 만들기 --------------------------------------------------------------------------------------------------------------
# graph형식 데이터셋 만들기
all_word_graph = co_occurence_df %>% 
  arrange(desc(cor_value)) %>% 
  top_n(500) %>%  
  graph_from_data_frame(directed = F)

# 전체 네트워크 맵 생성
ggraph(all_word_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", show.legend = FALSE, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "grey", size = centralization.degree(all_word_graph)$res) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# 각 노드별 네트워크 지표 구하기 --------------------------------------------------------------------------------------------------------------
# 연결 중심성
centralization.degree(all_word_graph)$res

# 매개 중심성
centralization.betweenness(all_word_graph)$res

# 고유벡터 중심성
centralization.evcent(all_word_graph)$vector

# 각 단어별 네트워크 지표 매핑
word_network = data.frame(word = vertex_attr(all_word_graph)[[1]],
                          centrality = centralization.degree(all_word_graph)$res, #연결 중심성
                          betweenness =centralization.betweenness(all_word_graph)$res, #매개 중심성
                          eigenvector = centralization.evcent(all_word_graph)$vector # 고유벡터 중심성
)


# 특정 단어들 네트워크 맵 만들기 --------------------------------------------------------------------------------------------------------------
multi_word = c("대한항공","갑질","처벌") # 단어 세팅

co_occurence_df %>%
  filter(word1 %in% multi_word | word2 %in% multi_word) %>% 
  top_n(50) %>% # 보여질 연관단어 개수
  graph_from_data_frame(directed = F) %>% # 그래프 형태의 데이터셋으로 변환
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()


# bigram 네트워크 맵 만들기 --------------------------------------------------------------------------------------------------------------
parsedData_df = readRDS("./raw_data/parsed_petition_data.RDS")
#parsed_data = readRDS("./raw_data/parsedData_noun.RDS")
stopWordDic = read_csv("./dictionary/stopword_ko.csv")

# 형태소 분석된 결과만 가져오기
parsed_text = as.character(parsedData_df$text)
bigram = vector(length = length(parsed_text))

# 전처리 
for (i in 1:length(parsed_text)) {
  # 형태소분석 결과 길이가 2보다 큰 경우
  if (nchar(parsed_text[i]) > 2) {  
    bi_list = sapply(ngrams(words(parsed_text[i]), 2), paste, collapse = " ") # bi-gram으로 자르기
    bi_list = paste(bi_list, collapse=',')
    bigram[i] = bi_list
  }
  ifelse(i %% 10000 == 0, print(i), next())
}

bigram_tb = str_split_fixed(bigram, ",", n = Inf)
bigram_tb = data.table(bigram_w = c(bigram_tb))
bigram_tb = bigram_tb[,.N, by=.(bigram_w)][order(-N)]

bigrams_separated = bigram_tb %>% separate(bigram_w, c("word1", "word2"), sep = " ")

bigrams_filtered = bigrams_separated %>%
  filter(!word1 %in% stopWordDic$stopword) %>% # 불용어 처리
  filter(!word2 %in% stopWordDic$stopword) %>%
  filter(nchar(word1)>1) %>% # 1글자 이상 단어만 추출
  filter(nchar(word2)>1)

# graph형식 데이터셋 만들기
bigram_graph = bigrams_filtered %>% arrange(desc(N)) %>% top_n(300) %>%  graph_from_data_frame()

# 화살표 및 그래프 스타일
a = grid::arrow(type = "closed", length = unit(.15, "inches"))
set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")

# 네트워크 맵 생성
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = N), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# 특정 단어 bigram 네트워크 맵 만들기 ----------------------------------------------------------------------------------------------------

bigrams_filtered %>%
  filter(word1 == "임금") %>%
  top_n(20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = N, edge_width = N), edge_colour = "cyan4", arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()


# bigram 단위 워드 클라우드 --------------------------------------------------------------------------------------------------------------
bigrams_filtered$concat_word = paste(bigrams_filtered$word1, bigrams_filtered$word2)
library(wordcloud2)

top100 = bigrams_filtered %>% select(concat_word, N) %>% top_n(100) # 상위 100개 단어만 추출

wordcloud2(data = top100
           , color = "random-light"
           , shape = "star"
           , size = 0.5
           , fontFamily = "나눔고딕")
