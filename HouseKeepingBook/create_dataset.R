rm(list=ls())
gc()

library(reshape2)
require(gdata)
require(stringi)
require(ggplot2)
require(dplyr)
require(reshape)
library(readxl)

#expenditureFileList = list.files("./dataFile/expenditure/")
#incomeFileList = list.files("./dataFile/income/")
expenditureFileList = list.files("~/Library/Mobile Documents/com~apple~CloudDocs/개인용/dataFile/expenditure/")
incomeFileList = list.files("~/Library/Mobile Documents/com~apple~CloudDocs/개인용/dataFile/income/")
data_dir_path = "~/Library/Mobile Documents/com~apple~CloudDocs/개인용/dataFile"

filePreProcessing <- function(expenditurFile, incomeFile){
  
  expenditure = NULL
  save = NULL
  income = NULL
  
  for(i in 1:length(expenditureFileList)){
    expenditureTemp = read_xls(paste0(data_dir_path,"/expenditure/",expenditureFileList[i]), sheet = 1)
    headName = t(expenditureTemp[3,])
    headName = headName[,1]
    expenditureTemp = expenditureTemp[4:(nrow(expenditureTemp)-4),]
    colnames(expenditureTemp) = headName
    colnames(expenditureTemp)[3] = "detail"
    expenditureTemp$카드 = as.numeric(expenditureTemp$카드)
    expenditureTemp$현금 = as.numeric(expenditureTemp$현금)
    
    saveTemp = expenditureTemp %>% dplyr::filter(grepl("저축/보험", 분류))
    expenditureTemp = expenditureTemp %>% dplyr::filter(!grepl("저축/보험", 분류))
    
    expenditure = rbind(expenditure, expenditureTemp)
    save = rbind(save, saveTemp)
  }
  
  for(i in 1:length(incomeFileList)){
    incomeTemp = read_xls(paste0(data_dir_path,"/income/",incomeFileList[i]), sheet =  1)
    headName = t(incomeTemp[3,])
    headName = headName[,1]
    incomeTemp = incomeTemp[4:(nrow(incomeTemp)-1),]
    colnames(incomeTemp) = headName
    incomeTemp$금액 = as.numeric(incomeTemp$금액)
    colnames(incomeTemp)[2] = "detail"
    
    income = rbind(income, incomeTemp)
  }
  
  #Data Processing - Expenditure
  expenditure$날짜 = stri_paste(substr(expenditure$날짜,1,4),"-",substr(expenditure$날짜,6,7),"-",substr(expenditure$날짜,9,10))
  expenditure$yearMonth = stri_paste(substr(expenditure$날짜,1,4),"-",substr(expenditure$날짜,6,7))
  expenditure$year = stri_paste(substr(expenditure$날짜,1,4))
  expenditure$totalExpend = as.numeric(expenditure$현금) + as.numeric(expenditure$카드)
  expenditure$category1 = stri_split_fixed(expenditure$분류,">",simplify = TRUE)[,1]
  expenditure$category2 = stri_split_fixed(expenditure$분류,">",simplify = TRUE)[,2]
  
  credit_amount = expenditure %>% dplyr::filter(category1 == "카드대금") 
  credit_amount = credit_amount %>% select(날짜, year, yearMonth, category1, category2, detail, 현금, 카드, 카드분류, totalExpend)
  colnames(credit_amount) = c("날짜", "year", "yearMonth", "category1", "category2", "detail", "현금", "카드", "카드분류", "total")
  
  expenditure = expenditure %>% dplyr::filter(category1 != "카드대금")
  expenditure = expenditure %>% select(날짜, year, yearMonth, category1, category2, detail, 현금, 카드, 카드분류, totalExpend)
  colnames(expenditure) = c("날짜", "year", "yearMonth", "category1", "category2", "detail", "현금", "카드", "카드분류", "total")
  expenditure$type = "expenditure"
  
  #Data Processing - Save
  save$날짜 = stri_paste(substr(save$날짜,1,4),"-",substr(save$날짜,6,7),"-",substr(save$날짜,9,10))
  save$yearMonth = stri_paste(substr(save$날짜,1,4),"-",substr(save$날짜,6,7))
  save$year = stri_paste(substr(save$날짜,1,4))
  save$totalSave = as.numeric(save$현금) + as.numeric(save$카드)
  save$category1 = stri_split_fixed(save$분류,">",simplify = TRUE)[,1]
  save$category2 = stri_split_fixed(save$분류,">",simplify = TRUE)[,2]
  save = save %>% select(날짜, year, yearMonth, category1, category2, detail, 현금, 카드, 카드분류, totalSave)
  colnames(save) = c("날짜", "year", "yearMonth", "category1", "category2", "detail", "현금", "카드", "카드분류", "total")
  save$type = "save"
  
  #Data Processing - Income
  income$날짜 = stri_paste(substr(income$날짜,1,4),"-",substr(income$날짜,6,7),"-",substr(income$날짜,9,10))
  income$yearMonth = stri_paste(substr(income$날짜,1,4),"-",substr(income$날짜,6,7))
  income$year = stri_paste(substr(income$날짜,1,4))
  income$category1 = stri_split_fixed(income$분류, ">", simplify = TRUE)[,1]
  income$category2 = stri_split_fixed(income$분류, ">", simplify = TRUE)[,2]
  income = income %>% dplyr::filter(!(category1 %in% c("전월이월","저축/보험")))
  income = income %>% select(날짜, year, yearMonth, category1, category2, detail, 금액) %>% mutate(카드=NA, 카드분류=NA, total=금액)
  colnames(income) = c("날짜", "year", "yearMonth", "category1", "category2", "detail", "현금", "카드", "카드분류", "total")
  income$type = "income"
  
  binded_data = rbind(expenditure, save, income)
  binded_data$category2 = paste0(binded_data$category1, "_", binded_data$category2)
  return(list(binded_data, credit_amount))
}

processing_result = filePreProcessing(expenditureFileList, incomeFileList)
accountBook = processing_result[[1]]
credit_amount = processing_result[[2]]

accountBook = reshape2::dcast(accountBook, 날짜+year+yearMonth+category1+category2+detail+현금+카드+카드분류+total+type ~ type, fun.aggregate = sum, value.var = "total", fill = 0)
colnames(accountBook)[12:14] = c("totalExpenditure", "totalIncome","totalSave")

accountBook$year = substr(accountBook$yearMonth, 1, 4)

write.csv(accountBook, "./accountBook.csv",row.names = F)
