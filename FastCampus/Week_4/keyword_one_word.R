cor_term
library(data.table)

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtmW_m = as.matrix(dtmW)
cor_termW = cor(dtmW_m) # 상관계수 매트릭스 초기화

#Edge 개수 조절하기
cor_termW[cor_termW < 0.05] = 0

temp_df = data.table(
  word1 = character(),
  word2 = character(),
  cor_value = numeric()
)
for(i in 1:length(rownames(cor_termW))){
  for(j in 1:length(colnames(cor_termW))){
    if(cor_termW[i,j] > 0 & cor_termW[i,j] < 1){
      add_row = list(rownames(cor_termW)[i], colnames(cor_termW)[j], cor_termW[i,j])
      temp_df = rbind(temp_df, add_row)
    }
  }
}


#특정 단어 중심의 네트워크
temp_df %>%
  filter(word1 == "폭행") %>%
  top_n(20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()




# graph형식 데이터셋 만들기
bigram_graph = temp_df %>% arrange(desc(cor_value)) %>% top_n(500) %>%  graph_from_data_frame(directed = F)

# 화살표 및 그래프 스타일
a = grid::arrow(type = "closed", length = unit(.15, "inches"))
set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")

# 전체 네트워크 맵 생성
ggraph(bigram_graph, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = cor_value), show.legend = FALSE, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



as_tbl
