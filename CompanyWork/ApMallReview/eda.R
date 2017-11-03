load("./forReview.RData")
library(ggplot2)
library(dplyr)

#raw_review2는 2016, 2017년 댓글만
#각 컬럼 확인
ggplot(raw_review2, aes(x=v_reg_channel)) + geom_bar(stat = "count")

# User별 작성 댓글 수
userFreq = raw_review2 %>% group_by(v_userid) %>% summarise(n = n()) %>% arrange(-n)
ggplot(userFreq, aes(x=reorder(v_userid, -n), y = n)) + geom_bar(stat = "identity")

# 상품별 댓글수 구하기
n_review = targetSet %>% group_by(v_productcd) %>% summarise(n_review = n(), avgScore = mean(n_recom_point))

#판매수와 반품수의 관계
raw_productInfo$backRatio = raw_productInfo$n_back_cnt/raw_productInfo$n_sale_cnt
raw_productInfo$backDiff = raw_productInfo$n_sale_cnt - raw_productInfo$n_back_cnt
raw_productInfo$priceDiff = raw_productInfo$n_list_price - raw_productInfo$n_price
raw_productInfo = merge(raw_productInfo, n_review, by = "v_productcd", all.x = T)

ggplot(raw_productInfo %>% filter(n_total_cnt > 1000 & substr(v_sale_dt,1,4)=='2017'), aes(x=n_total_cnt, y=backRatio)) + geom_point()
ggplot(raw_productInfo %>% filter(n_back_cnt < 5000 & substr(v_sale_dt,1,4)=='2017'), aes(x=avgScore, y=n_back_cnt)) + geom_point()

