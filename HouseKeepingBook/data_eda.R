library(ggplot2)
library(dplyr)
library(data.table)
source("../common_function.R")

# 월별 지출 항목 보기
in_yearMonth = '2018-11'
monthly_exp_category = accountBook %>% filter(yearMonth == in_yearMonth) %>% group_by(category1, category2) %>% summarise(totalExpenditure = sum(totalExpenditure))

ggplot(monthly_exp_category, aes(x=category1, y=totalExpenditure, fill = category2)) + geom_bar(stat = "identity") + scale_y_continuous(labels = point) + th



# 동월 비교 ----------------------------------------------
target_month = '10'

temp = accountBook %>% 
  filter(substr(accountBook$yearMonth, start = 6, stop = 7) == target_month) %>% 
  group_by(yearMonth) %>% 
  summarise(totalExpenditure = sum(totalExpenditure))

ggplot(temp, aes(x=yearMonth, y = totalExpenditure)) + geom_bar(stat = "identity")+ scale_y_continuous(labels = point)


# 각 월별로 Boxplot 및 라인 ----------------------------------------------
temp  = accountBook %>% group_by(yearMonth) %>% summarise(totalExpenditure = sum(totalExpenditure)) %>%
  mutate(month_t = substr(yearMonth, start = 6, stop = 7)
         ,year_t = substr(yearMonth, start = 1, stop = 4))

ggplot(temp, aes(x=month_t, y=totalExpenditure)) + geom_boxplot(outlier.shape = 1) + scale_y_continuous(labels = point, limits = c(0,10000000))

ggplot(temp, aes(x=month_t, y=totalExpenditure, group=year_t, color=year_t)) + geom_line() + geom_point() + scale_y_continuous(labels = point, limits = c(0,35000000))


# 년도별 지출 규모 ----------------------------------------------
temp = accountBook %>% group_by(year) %>% summarise(totalExpenditure = sum(totalExpenditure))
ggplot(temp, aes(x=year, y = totalExpenditure)) + geom_bar(stat = "identity")+ scale_y_continuous(labels = point, limits = c(0,65000000))


# 년월별 지출 규모 ----------------------------------------------
temp = accountBook %>% group_by(yearMonth) %>% summarise(totalExpenditure = sum(totalExpenditure))
ggplot(temp, aes(x=yearMonth, y = totalExpenditure)) + geom_bar(stat = "identity")+ scale_y_continuous(labels = point, limits = c(0,35000000))




# 년도별 수입 규모 ----------------------------------------------
temp = accountBook %>% group_by(year) %>% summarise(totalIncome = sum(totalIncome), totalMonthIncome = sum(totalIncome)/12)
ggplot(temp, aes(x=year, y = totalMonthIncome, label = round(totalMonthIncome, 1))) + geom_bar(stat = "identity")+ 
         scale_y_continuous(labels = point, limits = c(0,12000000)) + geom_text()

ggplot(temp, aes(x=year, y = totalIncome, label = round(totalIncome, 1))) + geom_bar(stat = "identity")+ 
  scale_y_continuous(labels = point, limits = c(0,120000000)) + geom_text()

       