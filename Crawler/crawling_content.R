#install.packages("rvest")
library(rvest)
library(stringr)
library(readr)
library(xml2)
library(tidyr)
library(dplyr)
library(data.table)

# 동의수가 100이상인 청원글만 선별
petitionList = petitionList_2018 %>% filter(agree_count >= 100)

#문서ID
petitionList$doc_id = str_extract(petitionList$link, pattern = "[0-9]{5,10}")

# content 가져오기
petition_content = data.frame(doc_id=as.character(), title=as.character(), content=as.character(), startDate=as.character(), endDate=as.character(), agreeCount=as.numeric())
for(i in 1:nrow(petitionList)){
  #for(i in 1:10){
  print(i)
  
  # 수집한 링크 읽어오기
  read_html_result = read_html(petitionList$link[i])
  
  #문서ID
  doc_id = str_extract(petitionList$link[i], pattern = "[0-9]{5,10}")
  
  # 제목 가져오기
  title = read_html_result %>% html_nodes("h3.petitionsView_title") %>% html_text()
  #title = trimws(unlist(str_split(trimws(title),"\r\n",2))[2])
  
  # 내용 가져오기
  content = read_html_result %>% html_nodes("div.View_write") %>% html_text()
  content = trimws(content)  
  content = str_replace_all(content, pattern = "[\t\r\n]", replacement = "")
  if(length(content)>1){
    content = paste(content, collapse = "")
  }
  
  # 날짜 가져오기
  startDate = read_html_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div/div[1]/div/div[1]/div/div[2]/ul/li[2]/text()") %>% html_text()
  endDate = read_html_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div/div[1]/div/div[1]/div/div[2]/ul/li[3]/text()") %>% html_text()
  
  # 동의수
  agreeCount = read_html_result %>% html_node(xpath = "//*[@id=\"cont_view\"]/div/div[1]/div/div[1]/div/h2/span") %>% html_text()
  agreeCount = str_replace_all(agreeCount, ",","")
  agreeCount = as.numeric(agreeCount)
  
  row = data.frame(doc_id, title, content, startDate, endDate, agreeCount, stringsAsFactors = F)
  petition_content = rbind(petition_content, row)
  Sys.sleep(runif(1,1,5))
}

petition_content_not_dup =petition_content[!duplicated(petition_content$doc_id),]
petition_content_not_dup = merge(petitionList, petition_content_not_dup, by = "doc_id")
saveRDS(petition_content_not_dup, "./petitions_content_2018.RDS")
