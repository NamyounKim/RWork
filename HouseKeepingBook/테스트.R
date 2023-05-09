
accountBook = data.table(accountBook)
accountBook$month_id = substr(accountBook$yearMonth, 6,7)

#월별 카테고리별 지출 비교
before_ym = '2022-10'
after_ym = '2022-11'

  
#지출 카테고리별 집계
before_monthly_exp_cat = accountBook[yearMonth %in% before_ym & type == 'expenditure'] %>% 
  group_by(category1) %>% 
  summarise(totalExpenditure = sum(totalExpenditure)) %>%
  mutate(expenditure_ratio = totalExpenditure/sum(totalExpenditure)) %>%
  arrange(-totalExpenditure) %>% as.data.table()

before_monthly_exp_cat$yearMonth = before_ym

after_monthly_exp_cat = accountBook[yearMonth %in% after_ym & type == 'expenditure'] %>% 
  group_by(category1) %>% 
  summarise(totalExpenditure = sum(totalExpenditure)) %>%
  mutate(expenditure_ratio = totalExpenditure/sum(totalExpenditure)) %>%
  arrange(-totalExpenditure) %>% as.data.table()
after_monthly_exp_cat$yearMonth = after_ym

rbind_monthly_exp_cat = rbind(before_monthly_exp_cat, after_monthly_exp_cat)

p = 
  ggplot(rbind_monthly_exp_cat, aes(x = category1, y = totalExpenditure, color = yearMonth, fill = yearMonth)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) + 
  scale_y_continuous(labels = point, breaks = breaks_extended(n = 10)) +
  theme(axis.text.x=element_text(size = 9, face = "bold")
        ,axis.text.y=element_text(size = 9, face = "bold")
        , plot.title = element_text(hjust = 0.5, face = "bold")
        , title = element_text(hjust = 0.5, size = 12, face = "bold")
        , legend.position = "top"
        , legend.text = element_text(size = 12, face = "bold")
        , text = element_text(family = "NanumBarunGothic"))
p  


cbind_monthly_exp_cat = merge(before_monthly_exp_cat, after_monthly_exp_cat, by = "category1", all = T)
#NA 체크
cbind_monthly_exp_cat$totalExpenditure.x[is.na(cbind_monthly_exp_cat$totalExpenditure.x)] = 0
cbind_monthly_exp_cat$totalExpenditure.y[is.na(cbind_monthly_exp_cat$totalExpenditure.y)] = 0
cbind_monthly_exp_cat$diff = cbind_monthly_exp_cat$totalExpenditure.y - cbind_monthly_exp_cat$totalExpenditure.x
cbind_monthly_exp_cat$change_rate = round(cbind_monthly_exp_cat$diff / cbind_monthly_exp_cat$totalExpenditure.x, digits = 2)
point(sum(cbind_monthly_exp_cat$diff))


ggplot(temp, aes(x = reorder(category2, totalExpenditure), y = totalExpenditure)) + 
  geom_bar(stat = "identity") + coord_flip() +
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
        , text = element_text(family = "NanumBarunGothic"))

plotly::ggplotly(p)

library(extrafont)
library(extrafontdb)
library(Rttf2pt1)
font_import()
font_import(paths = "/Users/namyun/Documents/Fonts")
loadfonts()
ttf_import(paths = "/Users/namyun/Documents/Fonts")
