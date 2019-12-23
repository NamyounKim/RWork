install.packages(c("tidyr","igraph","network","sna","GGally","data.table","ggraph"))
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
library(stringr)
library(readr)
library(wordcloud2)

#아래 코드를 수행하기 이전에 반드시 DTM을 생성해야 합니다.
#text_handling.R 소스코드 참고 하세요.
corp = readRDS("./data/corpus_petition.RDS")

#TF-IDF 값으로 연관 키워드 추출하기
dtm_tfidf = DocumentTermMatrix(corp
                               ,control=list(wordLengths=c(2,Inf)
                               ,weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기

# Remove sparse term
dtm_tfidf = removeSparseTerms(dtm_tfidf, sparse = 0.98)


#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtm_tfidf_mat = as.matrix(dtm_tfidf)
cor_term_tfidf = cor(dtm_tfidf_mat) # 상관계수 매트릭스 초기화

#Edge 개수 조절하기
cor_term_tfidf[cor_term_tfidf < 0.05] = 0
cor_term_tfidf[1,]


# 동시출현 단어 상관계수 데이터셋 만들기
co_occurence_df = data.table(
  word1 = character(),
  word2 = character(),
  cor_value = numeric()
)

for(i in c(1,2,3,4)){
  print(i*2)
}

for(i in 1:length(rownames(cor_term_tfidf))){
  t = i+1
  if(i == length(rownames(cor_term_tfidf))){
    break() # for문 종료
  }
  for(j in t:length(colnames(cor_term_tfidf))){
    if(cor_term_tfidf[i,j] > 0 & cor_term_tfidf[i,j] < 1){
      add_row = list(rownames(cor_term_tfidf)[i], colnames(cor_term_tfidf)[j], cor_term_tfidf[i,j])
      co_occurence_df = rbind(co_occurence_df, add_row)
    }
  }
}

# 나중에 shiny 대시보드 사용을 위해 저장
saveRDS(co_occurence_df,"./script/day2/shiny_code_new/co_occurence_df.rds")

# 스타일 설정
set_graph_style(family = "AppleGothic", face = "plain", text_size = 9, text_colour = "black")

# 특정 단어 중심의 네트워크 -------------------------------------------------------------------------------------------------------------------
one_word = "아파트" # 중심 단어 세팅
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

word_network %>% arrange(-betweenness) %>% head(5)


# 특정 단어들 네트워크 맵 만들기 --------------------------------------------------------------------------------------------------------------
multi_word = c("아파트","분양","택배") # 단어 세팅
#multi_word = c("어린이집","부모","원장") # 단어 세팅

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
parsed_df = readRDS("./data/parsed_petition_df.RDS")
stopword_dic = read_csv("./dictionary/stopword_ko.csv")

# 형태소 분석된 결과만 가져오기
parsed_text = as.character(parsed_df$text)

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

# 각 bigram 짝별로 count 세기
bigram_tb = bigram_tb %>% group_by(bigram_w) %>% summarise(N = n())

bigrams_separated = bigram_tb %>% separate(bigram_w, c("word1", "word2"), sep = " ")

bigrams_filtered = bigrams_separated %>%
  filter(!word1 %in% stopword_dic$stopword) %>% # 불용어 처리
  filter(!word2 %in% stopword_dic$stopword) %>%
  filter(nchar(word1)>1) %>% # 1글자 이상 단어만 추출
  filter(nchar(word2)>1)

# graph형식 데이터셋 만들기
bigram_graph = bigrams_filtered %>% arrange(desc(N)) %>% top_n(300) %>%  
  graph_from_data_frame() # bigram 네트워크맵은 유향 그래프 

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
  filter(word1 == "아파트") %>%
  top_n(20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = N, edge_width = N), edge_colour = "cyan4", arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()


