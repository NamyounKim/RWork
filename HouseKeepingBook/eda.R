rm(list=ls())
gc()

require(gdata)
require(stringi)
require(ggplot2)
require(dplyr)
require(reshape)

### Font Setting ###
library(extrafont)
font_import()
fonts()
loadfonts(device = "postscript")

#Load Expenditure Data
expenditure = read.xls("./raw_data.xlsx", sheet=1, header=TRUE, perl = "D:/Strawberry/perl/bin/perl5.22.2.exe")
expenditure = read.csv("./expenditure.csv")
head(expenditure)
expenditure = as.data.frame(expenditure)

#Load Income Data
income = read.xls("./raw_data.xlsx", sheet=2, header=TRUE)
income = read.csv("./income.csv")
head(income)
income = as.data.frame(income)

#Data Processing - Expenditure
expenditure$useDate = stri_paste(substr(expenditure$date,1,4),"-",substr(expenditure$date,6,7),"-",substr(expenditure$date,9,10))
expenditure$yearMonth = stri_paste(substr(expenditure$date,1,4),"-",substr(expenditure$date,6,7))
expenditure$totalExpend = expenditure$cash + expenditure$credit
expenditure$category1 = stri_split_fixed(expenditure$category,">",simplify = TRUE)[,1]
expenditure$category2 = stri_split_fixed(expenditure$category,">",simplify = TRUE)[,2]

subExpend = subset(expenditure, !(category1 %in% c('저축/보험','카드대금')))


#Data Processing - Income
income$incomeDate = stri_paste(substr(income$date,1,4),"-",substr(income$date,6,7),"-",substr(income$date,9,10))
income$yearMonth = stri_paste(substr(income$date,1,4),"-",substr(income$date,6,7))
income$category1 = stri_split_fixed(income$category, ">", simplify = TRUE)[,1]
income$category2 = stri_split_fixed(income$category, ">", simplify = TRUE)[,2]

subIncome = subset(income, !(category1 %in% c('저축/보험','전월이월')))

e_ym = subExpend %>% group_by(yearMonth, category1) %>% summarise(sumMoney=sum(totalExpend)) %>% mutate(type='expend')
i_ym = subIncome %>% group_by(yearMonth, category2) %>% summarise(sumMoney = sum(income)) %>% mutate(type='income')
e_category1 = subExpend %>% group_by(category1) %>% summarise(sumCash=sum(totalExpend))
i_category1 = subIncome %>% group_by(category1) %>% summarise(sumIncome=sum(income))


ei_ym = rbind(e_ym, i_ym)

ggplot(ei_ym, aes(x=yearMonth, y=sumMoney, group=type, colour=type)) + geom_point() + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(subset(e_ym, yearMonth<'2016-01'), aes(x=yearMonth, y=sumMoney, group=category1, colour=category1)) + geom_point() + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(i_ym, aes(x=yearMonth, y=sumMoney, group=category2, colour=category2)) + geom_point() + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(e_category1, aes(x=category1, y=sumCash)) + geom_bar(stat = "identity")
ggplot(i_category1, aes(x=category1, y=sumIncome)) + geom_bar(stat = "identity")


theme.text <- element_text(family="Apple SD Gothic Neo", size=10)
+ theme(axis.text.x=theme.text)
