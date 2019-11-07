library(textrank)
library(udpipe)
library(NLP4kec)
library(stringi)
library(tm)
library(readr)
data(joboffer)

joboffer$textrank_id <- unique_identifier(joboffer, c("doc_id", "paragraph_id", "sentence_id"))
candidates <- textrank_candidates_all(unique(joboffer$textrank_id))
head(candidates, 50)



keywords <- textrank_keywords(joboffer$lemma,
                              relevant = joboffer$upos %in% c("NOUN", "VERB", "ADJ"))
subset(keywords$keywords, ngram > 1 & freq > 1)

keywords <- textrank_keywords(joboffer$lemma,
                              relevant = joboffer$upos %in% c("NOUN"),
                              p = 1/2, sep = " ")
subset(keywords$keywords, ngram > 1)


#----------------------------------------------------------------------------------------------------------------------------
doc_sample = read_file("./sample/doc3")

sentences = unlist(stri_split_lines(doc_sample))

p_sentences = r_extract_noun(sentences, useEn = T, language = "ko", korDicPath = "./user_dic.txt")
p_sentences = as.data.frame(p_sentences)
p_sentences$setence_id = rownames(p_sentences)
p_sentences$p_sentences = as.character(p_sentences$p_sentences)


word_df = data.frame()
for(i in 1:nrow(p_sentences)){
  if(nchar(p_sentences[i,]$p_sentences) > 0){
    temp_df = data.frame(sentence_id = i
                         ,word = unlist(stri_split_fixed(p_sentences[i,]$p_sentences, pattern = " ", omit_empty = T)))
    word_df = rbind(word_df, temp_df)
  }
}
word_df$word = as.character(word_df$word)

keywords <- textrank_keywords(word_df$word, p = 0.1, sep = "-")
subset(keywords$keywords, ngram > 1 & freq > 1)
subset(keywords$keywords, ngram > 2)


subset(keywords$keywords, ngram > 1)
textrank::textrank_sentences()

#--------------------------------------------
textData = readRDS("/Users/kakao/Documents/GitHub/RWork/FastCampus/raw_data/petitions_content_2018.RDS")

sentences = unlist(stri_split_lines(textData$content[1]))
sentences
