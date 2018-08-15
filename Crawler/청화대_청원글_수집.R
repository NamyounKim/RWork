#install.packages("rvest")
library(rvest)
library(stringr)
library(readr)
library(xml2)
library(tidyr)
library(dplyr)
library(data.table)

# List 가져오기
petitionList = data.table(link=as.character(), category=as.character(), agree_count = as.numeric())
for(i in 6247:11000){
  Sys.sleep(runif(1,1,5))
  print(i)
  url = paste0("https://www1.president.go.kr/petitions?only=finished&page=",i)
  
  read_url_result = read_html(url)
  
  link = read_url_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div/div/div[1]/div/div/div[2]") %>% html_nodes("a.cb") %>% html_attr("href")
  #link = link[2:length(link)]
  category = read_url_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div/div/div[1]/div/div/div[2]") %>% html_nodes("div.bl_category.cs") %>% html_text()
  
  agree_count = read_url_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div/div/div[1]/div/div/div[2]") %>% html_nodes("div.bl_agree") %>% html_text()
  agree_count = str_replace_all(trimws(agree_count),pattern = "참여인원 ", replacement = "")
  agree_count = str_replace_all(agree_count, pattern = "명", replacement = "")
  agree_count = str_replace_all(agree_count, pattern = ",", replacement = "")
  agree_count = as.numeric(agree_count)
  
  rows = data.frame(link, category, agree_count, stringsAsFactors = F)
  petitionList = rbind(petitionList, rows)
  
  if(i %% 5000 == 0){
    saveRDS(petitionList, paste0("./petitionList_",i,".rds"))
  }
}

saveRDS(petitionList, paste0("./petitionList_",i,".rds"))
