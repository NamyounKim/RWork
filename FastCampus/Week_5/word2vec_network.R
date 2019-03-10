library(tm)
library(dplyr)
library(tidyr)
library(igraph)
library(network)
library(ggplot2)
library(GGally)
library(ggraph)
library(data.table)
library(stringi)
library(readr)

# corpus 가져오기
corp = readRDS("./raw_data/corpus_petition.RDS")

#TF-IDF 값으로 연관 키워드 추출하기
dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                             weighting = function(x) weightTfIdf(x, normalize = TRUE))) #Tf-Idf 가중치 주기
#Sparse Terms 삭제
dtmW = removeSparseTerms(dtmW, as.numeric(0.99))

#주요단어 리스트 (tfidf 기준으로 중요한 단어를 선별)
target_words = dtmW$dimnames$Terms

# 단어간 코사인 유사도 구하기
cosine_sim_mat = cosineSimilarity(model, model)

# Edge 개수 조절하기 (0~1 사이 값으로 세팅)
cosine_sim_mat[cosine_sim_mat < 0.6] = 0

# 코사인유사도에서 주요 단어만 남기기
sub_cosine_sim_mat = cosine_sim_mat[rownames(cosine_sim_mat) %in% target_words, colnames(cosine_sim_mat) %in% target_words]
dim(sub_cosine_sim_mat)

# 단어간 코사인 유사도 데이터셋 만들기
cosine_sim_df = data.table(
  word1 = character(),
  word2 = character(),
  cos_sim_value = numeric()
)
for(i in 1:length(rownames(sub_cosine_sim_mat))){
  t = i+1
  if(i == length(rownames(sub_cosine_sim_mat))){
    break()
  }
  for(j in t:length(colnames(sub_cosine_sim_mat))){
    if(sub_cosine_sim_mat[i,j] > 0 & sub_cosine_sim_mat[i,j] < 1){
      add_row = list(rownames(sub_cosine_sim_mat)[i], colnames(sub_cosine_sim_mat)[j], sub_cosine_sim_mat[i,j])
      cosine_sim_df = rbind(cosine_sim_df, add_row)
    }
  }
}

# 스타일 설정
set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")

# 특정 단어 중심의 네트워크 -------------------------------------------------------------------------------------------------------------------
one_word = "임금" # 중심 단어 세팅
one_word_graph = cosine_sim_df %>%
  filter(word1 == one_word | word2 == one_word) %>% 
  top_n(20) %>% # 보여질 연관단어 개수
  graph_from_data_frame(directed = F)# 그래프 형태의 데이터셋으로 변환

ggraph(graph = one_word_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = cos_sim_value, edge_width = cos_sim_value), edge_colour = "cyan4", end_cap = circle(.1, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()


# 전체 네트워크 맵 만들기 --------------------------------------------------------------------------------------------------------------------
# graph형식 데이터셋 만들기
all_word_graph = cosine_sim_df %>% 
  arrange(desc(cos_sim_value)) %>% 
  top_n(500) %>%  
  graph_from_data_frame(directed = F)

# 전체 네트워크 맵 생성
ggraph(all_word_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = cos_sim_value, edge_width = cos_sim_value), edge_colour = "cyan4", show.legend = FALSE, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "grey", size = centralization.degree(all_word_graph)$res) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



