library(dplyr)
library(data.table)
library(ggplot2)


#2. 입주시기에 따른 하자접수량 비교 (공공데이터 중 입주물량 데이터와 비교하여 전국 입주물량이 많을 경우 기능공 수급이 어려워져 품질이 나빠진다는 가정을 확인) -----------------------------------------------------------------------------------------
raw_data_dt = readRDS("./data/raw_data_dt.RDS")

#접수 날짜 처리
raw_data_dt$report_date = as.Date(raw_data_dt$접수일)
raw_data_dt$report_year_month = substr(raw_data_dt$접수일, 1, 7) #접수일의 년,월 정보만 추출

#월별 접수량
set1 = raw_data_dt %>% group_by(report_year_month) %>% summarise(report_n = n())

ggplot(set1, aes(x=report_year_month, y = report_n, group = 1)) + geom_line() + geom_point() + theme(axis.text.x=element_text(angle = 45, hjust = 1))

#입주 날짜 처리
raw_data_dt$move_date = as.Date(raw_data_dt$입주일)
raw_data_dt$move_year_month = substr(raw_data_dt$move_date, 1, 7) #접수일의 년,월 정보만 추출

set2 = raw_data_dt %>% dplyr::filter(!is.na(move_year_month)) %>% group_by(move_year_month) %>% summarise(move_n = n())
ggplot(set2, aes(x=move_year_month, y = move_n, group = 1)) + geom_line() + geom_point() + theme(axis.text.x=element_text(angle = 45, hjust = 1))

merge_set = merge(set1, set2, by.x = "report_year_month", by.y = "move_year_month")

# VOC접수일과 입주일 간의 상관관계
plot(merge_set[,2:3])
cor(merge_set[,2:3])
melt_merge_set = melt(data = merge_set, id.vars = "report_year_month", measure.vars = c("report_n","move_n"))
ggplot(melt_merge_set, aes(x=report_year_month, y = value, group = variable, color = variable)) + geom_line() + geom_point() + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


#3. 입주후 시기별 하자접수량 (입주 초기에 하자접수가 급격하게 증가하다가 어느시점부터 얼마나 줄어드는지) -----------------------------------------------------------------------------------------
# 접수일 - 입주일 구하기 (입주일로 부터 몇일 뒤에 접수가 일어나는지 확인)
raw_data_dt$diff_day = raw_data_dt$report_date - raw_data_dt$move_date

#분포 보기
quantile(raw_data_dt$diff_day, seq(0,1,0.1), na.rm = T) # 음수값 존재: 잘못된 입주일임

#음수값 제외하고 분포 보기
quantile(raw_data_dt$diff_day[raw_data_dt$diff_day>=0], seq(0,1,0.1), na.rm = T) 

#구간 만들기
install.packages("extrafont")
library(extrafont)
extrafont::loadfonts()

raw_data_dt = raw_data_dt %>% mutate(period = case_when(diff_day <= 7 ~ "a_1주일 미만"
                                                        ,diff_day > 7 & diff_day <= 30 ~ "b_1주일 ~ 1달"
                                                        ,diff_day > 30 & diff_day <= 60 ~ "c_1달 ~ 2달"
                                                        ,diff_day > 60 & diff_day <= 90 ~ "d_2달 ~ 3달"
                                                        ,diff_day > 90 & diff_day <= 180 ~ "e_3달 ~ 6달"
                                                        ,diff_day > 180 & diff_day <= 365 ~ "f_6달 ~ 1년"
                                                        ,diff_day > 365 & diff_day <= 730 ~ "g_1년 ~ 2년"
                                                        ,diff_day > 730 & diff_day <= 1095 ~ "h_2년 ~ 3년 이상"
                                                        ,diff_day > 1095 ~ "h_3년 이상"))

diff_day_count = raw_data_dt %>% dplyr::filter(diff_day >= 0) %>% group_by(period) %>% summarise(voc_n = n())
options(scipen = 100)
ggplot(diff_day_count, aes(x=period, y=voc_n)) + geom_bar(stat = "identity") + theme(text = element_text(family = "AppleGothic"))

