library(tidyr)
library(igraph)
library(ggraph)
library(data.table)
library(ggplot2)
library(stringi)
library(dplyr)
library(readr)

stopWordDic = read_csv("./dictionary/stopword_ko.csv")

# bigrams 기준 네트워크 맵 만들기 ---------------------------------------------------------------------------------------------------------------------
parsed_data = readRDS("./raw_data/parsed_data.RDS")
bigram = vector(length = length(parsed_data))

# 전처리 
for (i in 1:length(parsed_data)) {
  # 형태소분석 결과 길이가 2보다 큰 경우
  if (nchar(parsed_data[i]) > 2) {  
    bi_list = sapply(ngrams(words(parsed_data[i]), 2), paste, collapse = " ") # bi-gram으로 자르기
    bi_list = paste(bi_list, collapse=',')
    bigram[i] = bi_list
  }
  ifelse(i %% 10000 == 0, print(i), next())
}

bigram_tb = stri_split_fixed(bigram, ",", omit_empty = T, tokens_only = T)
bigram_tb = data.table(bigram_w = unlist(bigram_tb))
bigram_tb = bigram_tb[,.N, by=.(bigram_w)][order(-N)]

bigrams_separated <- bigram_tb %>% separate(bigram_w, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopWordDic$stopword) %>%
  filter(!word2 %in% stopWordDic$stopword) %>%
  filter(nchar(word1)>1) %>%
  filter(nchar(word2)>1)

# graph형식 데이터셋 만들기
bigram_graph = bigrams_filtered %>% arrange(desc(N)) %>% top_n(200) %>%  graph_from_data_frame()

# 화살표 및 그래프 스타일
a = grid::arrow(type = "closed", length = unit(.15, "inches"))
set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")

# 전체 네트워크 맵 생성
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = N), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# 전체 네트워크 맵 (edge 가중치 설정)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = N, edge_width = N), edge_colour = "cyan4", arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()


#특정 단어 중심의 네트워크
bigrams_filtered %>%
  filter(word1 == "어린이집") %>%
  top_n(20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = N, edge_width = N), edge_colour = "cyan4", arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
