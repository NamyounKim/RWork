library(tm)
library(slam)
library(dplyr)
library(NLP4kec)
library(readr)

#voc 원문 가져오기
raw_data_dt = readRDS("./data/raw_data_dt.RDS")

# 2015년도 VOC만 가져오기
raw_data_dt_2015 = raw_data_dt %>% dplyr::filter(접수일 >= '2015-01-01' & 접수일 <= '2015-12-31')

#동의어 / 불용어 사전 불러오기
stopword_dic = read_csv("./dictionary/stopword_ko.csv")
synonym_dic = read_csv("./dictionary/synonym.csv")

# 2. 형태소 분석 및 전처리----------------------------------------------------------------------------------------------------------------------------------------------------
#명사, 동사, 형용사만 추출
parsed_vec = r_parser_r(contentVector = raw_data_dt_2015$하자상세
                        ,language = "ko"
                        ,useEn = T
                        ,korDicPath = "./dictionary/user_dictionary.txt")

# 동의어 처리
parsed_vec = synonym_processing(parsedVector = parsed_vec
                                ,synonymDic = synonym_dic)


# Corpus에 doc_id를 추가하기 위한 데이터 프레임 만들기
parsed_df = data.frame(doc_id = rownames(raw_data_dt_2015)
                       ,text = parsed_vec)

saveRDS(parsed_df, file = "./data/parsed_voc_data_dt.RDS") # 나중 재사용을 위해 저장

#Corpus 생성
corp = VCorpus(DataframeSource(parsed_df))

#특수문자 제거
corp = tm_map(corp, removePunctuation)

#숫자 삭제
corp = tm_map(corp, removeNumbers)

#특정 단어 삭제
corp = tm_map(corp, removeWords, stopword_dic$stopword)

saveRDS(corp, file = "./data/corpus_voc.RDS") # 나중 재사용을 위해 저장


# 3. DTM 생성 및 Sparse Term 삭제 --------------------------------------------------------------------------------------------------------------------------------------------------
#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm_tfidf = DocumentTermMatrix(corp
                               ,control=list(wordLengths=c(2,Inf)
                               ,weighting = function(x) weightTfIdf(x, normalize = TRUE))) #TF-IDF 가중치 주기

dtm_tfidf = removeSparseTerms(dtm_tfidf, sparse = 0.997)

library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)
library(ggraph)
library(data.table)

#Network Map용 데이터 만들기 (단어 X 단어 상관계수 매트릭스 생성)
dtm_tfidf_mat = as.matrix(dtm_tfidf)
cor_term_tfidf = cor(dtm_tfidf_mat) # 상관계수 매트릭스 초기화

#Edge 개수 조절하기
cor_term_tfidf[cor_term_tfidf < 0.01] = 0


# 동시출현 단어 상관계수 데이터셋 만들기
co_occurence_df = data.table(
  word1 = character(),
  word2 = character(),
  cor_value = numeric()
)
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

# 스타일 설정
set_graph_style(family = "AppleGothic", face = "plain", size = 10, text_size = 9, text_colour = "black")

# 특정 단어 중심의 네트워크 ---------------------------------------------------------------------------------------------------------------------------------------
#one_word = "소음" # 중심 단어 세팅
one_word = "코킹"
one_word_graph = co_occurence_df %>%
  filter(word1 == one_word | word2 == one_word) %>% 
  top_n(20) %>% # 보여질 연관단어 개수
  graph_from_data_frame(directed = F)# 그래프 형태의 데이터셋으로 변환

ggraph(graph = one_word_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", end_cap = circle(.1, 'inches')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()


# 전체 네트워크 맵 만들기 ------------------------------------------------------------------------------------------------------------------------------------------
# graph형식 데이터셋 만들기
all_word_graph = co_occurence_df %>% 
  arrange(desc(cor_value)) %>% 
  top_n(300) %>%  
  graph_from_data_frame(directed = F)

# 전체 네트워크 맵 생성
ggraph(all_word_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = cor_value, edge_width = cor_value), edge_colour = "cyan4", show.legend = FALSE, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "grey", size = centralization.degree(all_word_graph)$res) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
