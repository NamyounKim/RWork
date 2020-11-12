library(ggplot2)
library(dplyr)
library(data.table)

library(DT)
library(ggthemr)
library(ggsci)
library(ggrepel)
library(readr)
library(scales)
library(stringi)
library(plotly)
source("../common_function.R")
ggthemr('fresh')

accountBook = data.table(accountBook)
accountBook$month_id = substr(accountBook$yearMonth, 6,7)

except_life_category = c("세금/이자_세금", "세금/이자_대출이자", "건강/문화_보장성보험","세금/이자_기타")


#1. 월별 수입, 지출 현황 ==============================================================================================
input_year = '2020'
input_month = '09'
input_ym = paste0(input_year, "-", input_month)

#총 수입
total_income = sum(accountBook[yearMonth == input_ym]$totalIncome)
total_income

#Only 나의 수입
my_income = sum(accountBook[yearMonth == input_ym & !(category2 %in% c("주수입_현아급여", "주수입_주원이")) & detail != "퇴직금 찾음"]$totalIncome)
my_income

#총 지출
total_expenditure = sum(accountBook[yearMonth == input_ym]$totalExpenditure)
total_expenditure

# 생활비 지출
real_expenditure = sum(accountBook[yearMonth == input_ym & !(category2 %in% except_life_category)]$totalExpenditure)
real_expenditure

#총 세이브
total_save = total_income - total_expenditure
total_save

#총 세이브 비율
save_ratio = (total_income - total_expenditure) / total_income
save_ratio




#카테고리1 별 지출 ------------------------------------------------------------------------------------------
monthly_exp_cat1 = accountBook[yearMonth == input_ym & type == 'expenditure'] %>% 
  group_by(category1) %>% 
  summarise(totalExpenditure = sum(totalExpenditure)) %>%
  mutate(expenditure_ratio = totalExpenditure/sum(totalExpenditure)) %>%
  arrange(-totalExpenditure)
monthly_exp_cat1

ggplot(monthly_exp_cat1, aes(x = reorder(category1, totalExpenditure), y = totalExpenditure)) + 
  geom_bar(stat = "identity", ) + coord_flip() +
  geom_text(aes(label = percent(expenditure_ratio, accuracy = 0.1)), hjust = 0.5) +
  scale_y_continuous(labels = point) +
  labs(x = "지출 카테고리", y = "지출액") +
  scale_color_startrek() +
  theme(axis.text.x=element_text(size = 11, face = "bold")
        ,axis.text.y=element_text(size = 11, face = "bold")
        , plot.title = element_text(hjust = 0.5, face = "bold")
        , title = element_text(hjust = 0.5, size = 12, face = "bold")
        , legend.position = "top"
        , legend.text = element_text(size = 12, face = "bold")
        , text = element_text(family = "Kakao Regular"))

#카테고리2 별 지출 ------------------------------------------------------------------------------------------
monthly_exp_cat2 = accountBook[yearMonth == input_ym & type == 'expenditure'] %>% 
  group_by(category2) %>% 
  summarise(totalExpenditure = sum(totalExpenditure)) %>%
  mutate(expenditure_ratio = totalExpenditure/sum(totalExpenditure)) %>%
  arrange(-totalExpenditure)
monthly_exp_cat2

ggplot(monthly_exp_cat2, aes(x = reorder(category2, totalExpenditure), y = totalExpenditure)) + 
  geom_bar(stat = "identity", ) + coord_flip() +
  geom_text(aes(label = percent(expenditure_ratio, accuracy = 0.1)), hjust = 0.5) +
  scale_y_continuous(labels = point) +
  labs(x = "지출 카테고리", y = "지출액") +
  scale_color_startrek() +
  theme(axis.text.x=element_text(size = 11, face = "bold")
        ,axis.text.y=element_text(size = 11, face = "bold")
        , plot.title = element_text(hjust = 0.5, face = "bold")
        , title = element_text(hjust = 0.5, size = 12, face = "bold")
        , legend.position = "top"
        , legend.text = element_text(size = 12, face = "bold")
        , text = element_text(family = "Kakao Regular"))

#카테고리별 수입 ------------------------------------------------------------------------------------------
monthly_income_cat = accountBook[yearMonth == input_ym & type == 'income'] %>% 
  group_by(category2) %>% 
  summarise(total_income = sum(totalIncome)) %>%
  mutate(income_ratio = total_income/sum(total_income)) %>%
  arrange(-total_income)
monthly_income_cat


#2. 년도별 수입, 지출 현황 ==============================================================================================
input_year = seq(2010, 2011, 1)

year_trend = accountBook %>% 
  group_by(yearMonth) %>% 
  summarise(t_income = sum(totalIncome)
            ,my_income = sum(if_else(category2 %in% c("주수입_현아급여", "주수입_주원이") | detail == "퇴직금 찾음", 0, totalIncome))
            ,t_exp = sum(totalExpenditure)
            ,life_exp = sum(if_else(category2 %in% except_life_category, 0, totalExpenditure))
            ,t_save = sum(totalIncome) - sum(totalExpenditure)) %>% 
  mutate(my_save = my_income - life_exp) %>% #나의 세이브 = 나의 수입 - 생활비 지출
  arrange(yearMonth) %>%
  mutate(t_cum_save = cumsum(t_save)) %>%
  data.table()

year_trend$year = substr(year_trend$yearMonth, 1, 4)
year_trend$my_save_ratio = year_trend$my_save / year_trend$my_income
year_trend$t_save_ratio = year_trend$t_save / year_trend$t_income

year_trend = year_trend[year %in% input_year]

year_trend_melt = melt.data.table(year_trend, id.vars = 'yearMonth'
                             ,measure.vars = c('t_exp','life_exp','t_income','my_income','t_save','t_cum_save','my_save_ratio','t_save_ratio')
                             ,variable.name = 'trend_type'
                             ,value.name = 'trend_value')


ggplot(year_trend_melt[trend_type %in% c('t_income','my_income','t_exp','life_exp','t_save')], aes(x = yearMonth, y = trend_value, group = trend_type, color = trend_type)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = point, breaks = seq(0, 50000000, 1000000)) +
  labs(x = "년월", y = "금액") +
  scale_color_startrek() +
  theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
        ,axis.text.y=element_text(size = 11, face = "bold")
        , plot.title = element_text(hjust = 0.5, face = "bold")
        , title = element_text(hjust = 0.5, size = 12, face = "bold")
        , legend.position = "top"
        , legend.text = element_text(size = 12, face = "bold")
        , text = element_text(family = "Kakao Regular"))


ggplot(year_trend_melt[trend_type %in% c('t_cum_save')], aes(x = yearMonth, y = trend_value/100000000, group = trend_type, color = trend_type)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = point, breaks = seq(0, 10, 0.1)) +
  labs(x = "년월", y = "금액(억)") +
  scale_color_startrek() +
  theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
        ,axis.text.y=element_text(size = 11, face = "bold")
        , plot.title = element_text(hjust = 0.5, face = "bold")
        , title = element_text(hjust = 0.5, size = 12, face = "bold")
        , legend.position = "top"
        , axis.ticks.x = element_blank()
        , legend.text = element_text(size = 12, face = "bold")
        , panel.grid.major.x = element_blank()
        , text = element_text(family = "Kakao Regular"))

ggplot(year_trend_melt[trend_type %in% c('t_save_ratio', 'my_save_ratio')], aes(x = yearMonth, y = trend_value, group = trend_type, color = trend_type)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent, breaks = extended_breaks(10)) +
  geom_hline(yintercept = 0.5, colour = "blue") +
  geom_hline(yintercept = 0, colour = "black") +
  labs(x = "년월", y = "save_ratio") +
  scale_color_startrek() +
  theme(axis.text.x=element_text(size = 11, face = "bold", angle = 45, hjust = 1)
        ,axis.text.y=element_text(size = 11, face = "bold")
        , plot.title = element_text(hjust = 0.5, face = "bold")
        , title = element_text(hjust = 0.5, size = 12, face = "bold")
        , legend.position = "top"
        , axis.ticks.x = element_blank()
        , legend.text = element_text(size = 12, face = "bold")
        , text = element_text(family = "Kakao Regular"))


