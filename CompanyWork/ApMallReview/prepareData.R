library(dplyr)

options(java.parameters = "-Xmx200g")
source("~/rwork/createImpalaConnection.R")
connImpala = getImpalaConnection()

# review 데이터 가져오기
query = 
"SELECT 
 o.v_reviewcd
,o.v_typecd
,o.v_userid
,o.v_ordercd
,o.v_youtube
,o.n_recom_point
,o.v_flag_rebuy -- 재구매 할거예요
,o.v_flag_prebuy -- 재구매 했어요.
,o.v_flag_best
,o.n_vote_cnt
,o.n_view_cnt
,o.v_content
,o.v_reg_channel
,o.v_reg_dtm
FROM bdsdb.o_cmc_review o"
raw_review = dbGetQuery(connImpala, query)

# 상품-리뷰 ID 가져오기
query = 
  "SELECT 
  o.v_reviewcd
 ,o.v_productcd
 ,o.v_optioncd
FROM bdsdb.o_cmc_review_prod o
WHERE v_reg_type='USER'"
raw_reviewXproudct = dbGetQuery(connImpala, query)

#날짜 생성
raw_review2 = raw_review %>% dplyr::select(v_reviewcd, v_userid, v_ordercd, n_recom_point, v_flag_rebuy, v_flag_prebuy, v_flag_best, v_content, v_reg_channel, v_reg_dtm)
raw_review2 = raw_review2 %>% mutate(regYear = substr(v_reg_dtm,1,4)
                                   ,regMonth = substr(v_reg_dtm,5,6)
                                   ,regDay = substr(v_reg_dtm,7,8)
                                   ,regHour = substr(v_reg_dtm,9,10))

# 리뷰데이터에 온라인 상품ID, 옵션ID 붙이기
raw_review2 = merge(raw_review2, raw_reviewXproudct, by = "v_reviewcd", all.x = T)
raw_review2 = raw_review2 %>% filter(regYear %in% c(2016,2017))



#상품 기본정보 가져오기
query = 
"SELECT
o.v_productcd
,o.v_productnm
,o.v_brandcd
,o.v_typecd
,o.v_statuscd
,o.n_list_price
,o.n_price
,o.n_vip_price
,o.v_coupon_yn
,o.v_reg_dtm
,o.v_flag_bpshop_open
FROM bdsdb.o_shop_product o
WHERE o.v_typecd != '0002'
AND o.n_price is not NULL
;"
raw_product = dbGetQuery(connImpala, query)

#옵션정보 가져오기
query = 
"SELECT
o.v_productcd
,o.v_optioncd
,o.v_optionnm
FROM bdsdb.o_shop_product_option o
WHERE o.n_list_price is not NULL
;"
raw_productOption = dbGetQuery(connImpala, query)
raw_productOption$newId = paste0(raw_productOption$v_productcd, raw_productOption$v_optioncd)
raw_productOption = raw_productOption %>% dplyr::select(newId, v_optioncd, v_optionnm)

#상품 통계정보 가져오기
query = 
"SELECT
o.v_productcd
,o.n_total_cnt
,o.n_sale_cnt
,o.n_back_cnt
,o.n_view_cnt
,o.n_vote_cnt
,o.v_sale_dt -- 마지막 판매일
,o.v_pre_sale_dt -- 마지막 이전 판매일
FROM bdsdb.o_shop_product_stats o
;"
raw_productStat = dbGetQuery(connImpala, query)

raw_productInfo = merge(raw_product, raw_productStat, by="v_productcd", all.x = T)
raw_productInfo = raw_productInfo %>% filter(!is.na(n_total_cnt) & n_total_cnt > 0)
raw_productInfo %>% arrange(-n_back_cnt) %>% select(v_productnm)


# 댓글 + 상품 + 상품옵션 정보
targetSet = merge(raw_review2, raw_productInfo, by = "v_productcd", all.x = T)
targetSet$newId = paste0(targetSet$v_productcd, targetSet$v_optioncd)
targetSet = merge(targetSet, raw_productOption, by = "newId", all.x = T)

# 댓글 길이 붙이기
targetSet$contentLength = nchar(targetSet$v_content)
targetSet$contentLength[is.na(targetSet$contentLength)] = 0

# 형태소 분석용
targetSetForParser = targetSet %>% dplyr::select(v_reviewcd, v_content)
colnames(targetSetForParser) = c("id","content")
targetSetForParser$content = gsub(",","",targetSetForParser$content)
write.csv(targetSetForParser, file = "./targetSetForParser.csv", quote = F, row.names = F, fileEncoding = "UTF-8")

