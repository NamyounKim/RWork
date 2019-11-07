#install.packages("rvest")
library(rvest)
library(stringr)
library(readr)
library(xml2)
library(tidyr)
library(dplyr)
library(data.table)

list_n = 10
file_name = "test"

# List 가져오기
petitionList_2019 = data.table(link=as.character(), category=as.character(), agree_count = as.numeric())

for(i in 1:list_n){
  Sys.sleep(runif(1,1,5))
  print(i)
  url = paste0("https://www1.president.go.kr/petitions?only=finished&page=",i)
  
  read_url_result = read_html(url)
  
  temp  =read_url_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div[2]/div/div/div[2]/div[2]/div[4]/div/div[2]")
  temp
  temp[[1]] %>% html_nodes("div") %>% html_nodes("div") %>% html_nodes("div.bl_subject")
  
  //*[@id="cont_view"]/div[2]/div/div/div[2]/div[2]/div[4]/div/div[2]/div[2]/ul/li[1]
  
  link = read_url_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div[2]/div/div/div[2]/div[2]/div[4]/div/div[2]/div[2]/ul") %>% html_nodes("a.cb.relpy_w") %>% html_attr("href")
  //*[@id="cont_view"]/div[2]/div/div/div[2]/div[2]/div[4]/div/div[2]/div[2]/ul/li[1]/div/div[3]/a
  
  print(link)
  #link = link[2:length(link)]
  category = read_url_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div/div/div[1]/div/div/div[2]") %>% html_nodes("div.bl_category.cs") %>% html_text()
  
  agree_count = read_url_result %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div/div/div[1]/div/div/div[2]") %>% html_nodes("div.bl_agree") %>% html_text()
  agree_count = str_replace_all(trimws(agree_count),pattern = "참여인원 ", replacement = "")
  agree_count = str_replace_all(agree_count, pattern = "명", replacement = "")
  agree_count = str_replace_all(agree_count, pattern = ",", replacement = "")
  agree_count = as.numeric(agree_count)
  
  rows = data.frame(link, category, agree_count, stringsAsFactors = F)
  petitionList_2019 = rbind(petitionList_2019, rows)
  
  if(i %% 5000 == 0){
    saveRDS(petitionList_2019, paste0("./petitionList_",file_name,"_",i,".rds"))
  }
}

saveRDS(petitionList_2019, paste0("./petitionList_",file_name,"_",i,".rds"))
