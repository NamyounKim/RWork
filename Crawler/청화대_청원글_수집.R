#install.packages("rvest")
library(rvest)
library(stringr)
library(readr)
library(xml2)

# List 가져오기
petitionList = data.frame(link=as.character(), category=as.character())
for(i in 1:500){
  Sys.sleep(runif(1,1,7))
  print(i)
  url = paste0("https://www1.president.go.kr/petitions?only=finished&page=",i)
  
  link = read_html(url) %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div[2]/div/div[1]/div/div/div[2]") %>% html_nodes("a.cb") %>% html_attr("href")
  #link = link[2:length(link)]
  category = read_html(url) %>% html_nodes(xpath = "//*[@id=\"cont_view\"]/div[2]/div/div[1]/div/div/div[2]") %>% html_nodes("div.bl_category.cs") %>% html_text()
  
  rows = data.frame(link, category, stringsAsFactors = F)
  petitionList = rbind(petitionList, rows)
}

# content 가져오기
petitions = data.frame(title=as.character(), content=as.character(), startDate=as.character(), endDate=as.character(), agreeCount=as.numeric())
for(i in 1:nrow(petitionList)){
#for(i in 1:10){
  print(i)
  
  title = read_html(petitionList$link[i]) %>% html_nodes("h5.big.bold.cb") %>% html_text()
  title = trimws(unlist(str_split(trimws(title),"\r\n",2))[2])
  
  content = read_html(petitionList$link[i]) %>% html_nodes("div.cspv_contents.text") %>% html_text()
  content = trimws(content)
  
  temp = read_html(petitionList$link[i]) %>% html_nodes("span.light") %>% html_text()
  startDate = as.Date(temp[1])
  endDate = as.Date(temp[2])
  
  agreeCount = read_html(petitionList$link[i]) %>% html_node(xpath = "//*[@id=\"cont_view\"]/div[2]/div/div/div/div[2]/div[2]/div[2]/h4/mark") %>% html_text()
  agreeCount = str_replace_all(agreeCount, ",","")
  agreeCount = as.numeric(agreeCount)
  
  row = data.frame(title, content, startDate, endDate, agreeCount, stringsAsFactors = F)
  petitions = rbind(petitions, row)
  Sys.sleep(runif(1,1,7))
}


petitions = cbind(petitionList, petitions)
saveRDS(petitions, "./petitions_finished.RDS")
