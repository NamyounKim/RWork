
#추출하고 싶은 토픽 번호
select_topic = c(1,2,9)

#선택한 토픽번호를 갖는 문서 추출
sub_corpus = id_topic %>% filter(doc_topic %in% select_topic) %>% dplyr::select(rown, parsedData)

#추출한 후 column명 변경
colnames(sub_corpus) = c("id", "parsedContent")

#shiny에 입력할 수 있는 csv형태의 파일로 떨구기
write.table(sub_corpus, "./sub_1_2_9.csv", sep=",", row.names=FALSE)
