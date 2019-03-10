library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

source("./tm_function.R") # sub_corpus로 DTM을 만드는 함수

#추출하고 싶은 토픽 번호
select_topic = c(16)

#선택한 토픽번호를 갖는 문서 추출
sub_parsedData = id_topic %>% filter(doc_topic %in% select_topic) %>% filter(maxProb > 0.4) %>% dplyr::select(text, doc_topic)

#------------------------------------------------------------------------------------
# sub_corpus로 DTM을 만드는 함수
dtm = makeDtm(parsedData = sub_parsedData$text, sr = 0.97, type = "tf-idf")
#------------------------------------------------------------------------------------

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtm_m = as.matrix(dtm)
cor_termW = cor(dtm_m)

#Edge 개수 조절하기
cor_termW[cor_termW < 0.4] = 0

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

# 스타일 설정
set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")



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
