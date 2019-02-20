library(ggplot2)
library(dplyr)
library(data.table)
source("../common_function.R")

# 월별 지출 항목 보기
in_yearMonth = '2018-11'
monthly_exp_category = accountBook %>% filter(yearMonth == in_yearMonth) %>% group_by(category1, category2) %>% summarise(totalExpenditure = sum(totalExpenditure))

ggplot(monthly_exp_category, aes(x=category1, y=totalExpenditure, fill = category2)) + geom_bar(stat = "identity") + scale_y_continuous(labels = point) + th



# 동월 비교
target_month = '10'

temp = accountBook %>% 
  filter(substr(accountBook$yearMonth, start = 6, stop = 7) == target_month) %>% 
  group_by(yearMonth) %>% 
  summarise(totalExpenditure = sum(totalExpenditure))

ggplot(temp, aes(x=yearMonth, y = totalExpenditure)) + geom_bar(stat = "identity")+ scale_y_continuous(labels = point)


# 각 월별로 Boxplot
temp  = accountBook %>% group_by(yearMonth) %>% summarise(totalExpenditure = sum(totalExpenditure)) %>%
  mutate(month_t = substr(yearMonth, start = 6, stop = 7))

ggplot(temp, aes(x=month_t, y=totalExpenditure)) + geom_boxplot(outlier.shape = 1) + scale_y_continuous(labels = point, limits = c(0,10000000))
