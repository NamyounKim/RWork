library(ggplot2)
library(dplyr)
library(data.table)
source("/Users/kakao/Documents/GitHub/RWork/common_function.R")

# 월별 지출 항목 보기
in_yearMonth = '2018-11'
monthly_exp_category = accountBook %>% filter(yearMonth == in_yearMonth) %>% group_by(category1, category2) %>% summarise(totalExpenditure = sum(totalExpenditure))

ggplot(monthly_exp_category, aes(x=category1, y=totalExpenditure, fill = category2)) + geom_bar(stat = "identity") + scale_y_continuous(labels = point) + th



# 동월 비교 ----------------------------------------------
target_month = "08"

temp = accountBook %>% 
  filter(substr(accountBook$yearMonth, start = 6, stop = 7) == target_month) %>% 
  group_by(yearMonth) %>% 
  summarise(totalExpenditure = sum(totalExpenditure))

ggplot(temp, aes(x=yearMonth, y = totalExpenditure)) + 
  geom_bar(stat = "identity")+ 
  scale_y_continuous(labels = point) + th


# 각 월별로 Boxplot 및 라인 ----------------------------------------------
temp  = accountBook %>% group_by(yearMonth) %>% summarise(totalExpenditure = sum(totalExpenditure)) %>%
  mutate(month_t = substr(yearMonth, start = 6, stop = 7)
         ,year_t = substr(yearMonth, start = 1, stop = 4))

ggplot(temp, aes(x=month_t, y=totalExpenditure)) + geom_boxplot(outlier.shape = 1) + scale_y_continuous(labels = point, limits = c(0,10000000))

ggplot(temp, aes(x=month_t, y=totalExpenditure, group=year_t, color=year_t)) + geom_line() + geom_point() + scale_y_continuous(labels = point, limits = c(0,35000000))


# 년도별 지출 규모 ----------------------------------------------
temp = accountBook %>% group_by(year) %>% summarise(totalExpenditure = sum(totalExpenditure))
ggplot(temp, aes(x=year, y = totalExpenditure, label = , label = point(round(totalExpenditure, 1)))) + 
  geom_bar(stat = "identity")+ 
  geom_text( position=position_stack(0.5)) + 
  scale_y_continuous(labels = point, limits = c(0,65000000))


# 년월별 지출 규모 ----------------------------------------------
temp = accountBook %>% group_by(yearMonth) %>% summarise(totalExpenditure = sum(totalExpenditure))
ggplot(temp, aes(x=yearMonth, y = totalExpenditure)) + geom_bar(stat = "identity")+ scale_y_continuous(labels = point, limits = c(0,35000000))




# 년도별 수입 규모 ----------------------------------------------
temp = accountBook %>% group_by(year) %>% summarise(totalIncome = sum(totalIncome), totalMonthIncome = sum(totalIncome)/12)
ggplot(temp, aes(x=year, y = totalMonthIncome, label = round(totalMonthIncome, 1))) + 
  geom_bar(stat = "identity") + 
  geom_text(position=position_stack(0.5)) +
  scale_y_continuous(labels = point, limits = c(0,12000000))

ggplot(temp, aes(x=year, y = totalIncome, label = point(round(totalIncome, 1)))) + 
  geom_bar(stat = "identity") + 
  geom_text(size = 3, vjust = -0.1) +
  scale_y_continuous(labels = point, limits = c(0,120000000))



# 년도별 신용카드 사용 규모 ----------------------------------------------

expenditure = NULL

for(i in 1:length(expenditureFileList)){
  expenditureTemp = read_xls(paste0(data_dir_path,"/expenditure/",expenditureFileList[i]), sheet = 1)
  headName = t(expenditureTemp[3,])
  headName = headName[,1]
  expenditureTemp = expenditureTemp[4:(nrow(expenditureTemp)-4),]
  colnames(expenditureTemp) = headName
  colnames(expenditureTemp)[3] = "detail"
  expenditureTemp$카드 = as.numeric(expenditureTemp$카드)
  expenditureTemp$현금 = as.numeric(expenditureTemp$현금)
  
  expenditureTemp = expenditureTemp %>% filter(!grepl("저축/보험", 분류))
  
  expenditure = rbind(expenditure, expenditureTemp)
}
expenditure$날짜 = stri_paste(substr(expenditure$날짜,1,4),"-",substr(expenditure$날짜,6,7),"-",substr(expenditure$날짜,9,10))
expenditure$yearMonth = stri_paste(substr(expenditure$날짜,1,4),"-",substr(expenditure$날짜,6,7))
expenditure$year = stri_paste(substr(expenditure$날짜,1,4))
expenditure$totalExpend = as.numeric(expenditure$현금) + as.numeric(expenditure$카드)
expenditure$category1 = stri_split_fixed(expenditure$분류,">",simplify = TRUE)[,1]
expenditure$category2 = stri_split_fixed(expenditure$분류,">",simplify = TRUE)[,2]

expenditure = expenditure %>% filter(category1 == "카드대금")
expenditure = expenditure %>% select(날짜, year, yearMonth, category1, category2, detail, 현금, 카드, 카드분류, totalExpend)

temp = expenditure %>% group_by(year) %>% summarise(use_credit_card = sum(totalExpend))

ggplot(temp, aes(x=year, y = use_credit_card, label = round(use_credit_card, 1))) + 
  geom_bar(stat = "identity") + 
  geom_text(position=position_stack(0.5))
