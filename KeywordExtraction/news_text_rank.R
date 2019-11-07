library(textrank)
library(NLP4kec)
library(stringi)
library(tm)
library(readr)
library(data.table)
library(tokenizers)

news_text = read_lines("./sample/180801.tsv")

news_text_list = stri_split_fixed(news_text, pattern = "\t")


#temp = do.call(news_text_list, as.data.table())
news_text_tb = do.call(rbind.data.frame, news_text_list)
colnames(news_text_tb) = c("dsid", "create_date", "cate_nm1", "cate_nm2", "title", "content", "cp_name", "news_type")
head(news_text_tb)


#전처리
news_text_tb = as.data.table(news_text_tb)
news_text_tb$p_content = stri_replace_all_regex(news_text_tb$content, pattern = "\\([가-힣A-Z0-9=,@&#]+\\)", replacement = "")
news_text_tb$p_content = stri_replace_all_regex(news_text_tb$p_content, pattern = "\\<[가-힣A-Z0-9=,@&#© ]+\\>", replacement = "")
news_text_tb$p_content = stri_replace_all_regex(news_text_tb$p_content, pattern = "\\[[가-힣A-Z0-9=,@&#© ]+\\]", replacement = "")
news_text_tb$p_content = stri_replace_all_regex(news_text_tb$p_content, pattern = "[A-Za-z0-9_.-]+@[A-Za-z0-9-]+.[A-Za-z0-9-]+", replacement = "")

#기자 단어 제거
news_text_tb$p_content = stri_replace_all_regex(news_text_tb$p_content, pattern = "[가-힣]+ 기자", replacement = "")


sentences_list = tokenize_sentences(news_text_tb$p_content, strip_punct = F)

names(sentences_list) = news_text_tb$dsid

sentences_dt = NULL
for(i in 1:length(sentences_list)){
  temp_sentence = unlist(sentences_list[i], use.names = F)
  
  if(length(temp_sentence) > 0){
    temp_dt = data.table(dsid = names(sentences_list[i])
                         ,sentence = temp_sentence)
    sentences_dt = rbind(sentences_dt, temp_dt)
  }
  #  print(i)
}

#문장별 테이블에 제목 붙이기
title_dt = news_text_tb[,.(dsid, title)]
colnames(title_dt)[2] = "sentence"
sentences_dt = rbind(sentences_dt, title_dt)


sentences_dt = merge(sentences_dt, news_text_tb[,.(dsid, cp_name, news_type)], by = "dsid", all.x = T)
sentences_dt$sentence = as.character(sentences_dt$sentence)

#형태소 분석
#parsed_sentences_dt = r_extract_noun(sentences_dt$sentence, useEn = T, language = "ko", korDicPath = "./user_dic.txt")
parsed_sentences_dt = r_parser_r(sentences_dt$sentence, useEn = T, language = "ko", korDicPath = "./user_dic.txt")
parsed_sentences_dt = as.data.frame(parsed_sentences_dt)
colnames(parsed_sentences_dt) = "parsed_sentence"
parsed_sentences_dt$parsed_sentence = as.character(parsed_sentences_dt$parsed_sentence)
parsed_sentences_dt = cbind(sentences_dt[,.(dsid, cp_name, news_type)], parsed_sentences_dt)
#p_sentences$setence_id = rownames(p_sentences)

# 문서별 문장별 단어 리스트 만들기
word_list = NULL
index = 0
for(did in unique(parsed_sentences_dt$dsid)){
  temp_dt = parsed_sentences_dt[dsid == did]
  index = index + 1
  #단어 단위로 쪼개서 저장
  word_dt = NULL
  
  for(i in 1:nrow(temp_dt)){
    if(nchar(temp_dt[i,]$parsed_sentence) > 0){
      word_temp_dt = data.table(dsid = did
                               ,sentence_id = i
                               ,word = unlist(stri_split_fixed(temp_dt[i,]$parsed_sentence, pattern = " ", omit_empty = T)))
      word_dt = rbind(word_dt, word_temp_dt)
    }
  }
  word_list[index] = list(word_dt)
}

word_all_dt = rbindlist(word_list)

# 텍스트 랭크 단어 추출
text_rank_list = NULL
index = 0
for(did in unique(parsed_sentences_dt$dsid)){
  index = index + 1
  keywords = textrank_keywords(word_all_dt[dsid == did]$word, p = 0.1, sep = "-")
  
  # 텍스트 랭크 단언 붙이기 (중복 제거)
  unique_keywords = unique(unlist(stri_split_fixed(subset(keywords$keywords, ngram > 1)$keyword, pattern = "-")))
  
  temp_dt = data.table(dsid = did
                       ,text_rank_word = paste0(unique_keywords, collapse = ","))
  
  text_rank_list[index] = list(temp_dt)
}

text_rank_dt = rbindlist(text_rank_list)

news_text_tb = merge(news_text_tb, text_rank_dt, by = "dsid", all.x = T)

#temp = news_text_tb[,.(dsid, content, text_rank_word)]


