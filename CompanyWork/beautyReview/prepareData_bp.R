dyn.load("/usr/java/jdk1.8.0_151/jre/lib/amd64/server/libjvm.so")
library(dplyr)
library(stringi)

options(java.parameters = "-Xmx200g")
source("~/rwork/createImpalaConnection.R")
connImpala = getImpalaConnection()

# review 데이터 가져오기
query = 
"SELECT 
  b.review_no,
  b.review_kind,
  b.review_title,
  b.reviewer,
  b.sex,
  b.age_group,
  b.skin_tone,
  b.skin_bright,
  b.skin_trouble,
  b.review_strength,
  b.review_weakness,
  b.review_etc,
  b.product_cd,
  b.product_nm,
  b.brand_cd,
  b.site_cd,
  b.satisfaction,
  b.hashtags,
  b.like_cnt,
  b.comment_cnt,
  b.share_cnt,
  b.view_cnt,
  b.give_point_val,
  b.use_yn,
  b.admin_yn,
  b.input_cstmid,
  b.input_ucstmid,
  b.input_date,
  b.update_cstmid,
  b.update_ucstmid,
  b.update_date,
  b.etlworktime
FROM lake_legacy_ods.bpa_bp_review b;"
raw_review = dbGetQuery(connImpala, query)

#날짜 생성
raw_review = raw_review %>% mutate(regYear = substr(input_date,1,4)
                                     ,regMonth = substr(input_date,5,6)
                                     ,regDay = substr(input_date,7,8)
                                     ,regHour = substr(input_date,9,10))

raw_review$new_site_cd = substr(raw_review$product_cd,1,3)
raw_review$new_product_cd = substr(raw_review$product_cd,4,nchar(raw_review$product_cd))

#상품 기본정보 가져오기
query = 
  "SELECT 
   p.ap_prd_cd as v_productcd,
   p.cem_prd_nm_n,
   p.brand_nm
FROM campdb.product_master_not_dup_for_apmall p
WHERE p.n_price is not NULL;"
raw_product = dbGetQuery(connImpala, query)

raw_review = merge(raw_review, raw_product, by.x = "new_product_cd", by.y = "v_productcd", all.x = T)
raw_review$brand_nm[raw_review$new_site_cd=="AIK"] = "이니스프리 본품"
raw_review$brand_nm[raw_review$new_site_cd=="ART"] = "아리따움"
raw_review$brand_nm[raw_review$new_site_cd=="EDK"] = "에뛰드하우스"
raw_review$brand_nm[raw_review$new_site_cd=="CAK"] = "오설록"

raw_review %>% group_by(brand_nm) %>% summarise(n=n()) %>% arrange(-n)
#raw_review_CMC = raw_review %>% filter(new_site_cd == "CMC")
raw_review_CMC = raw_review

#qcProgress(raw_review,1)

# NA 제거 및 형태소 분석용 SET 만들기
raw_review_strength =  raw_review_CMC %>% dplyr::select(review_no, review_strength, brand_nm) %>% filter(!is.na(review_strength))
raw_review_weakness =  raw_review_CMC %>% dplyr::select(review_no, review_weakness, brand_nm) %>% filter(!is.na(review_weakness))
raw_review_etc =  raw_review_CMC %>% dplyr::select(review_no, review_etc, brand_nm) %>% filter(!is.na(review_etc))

#강점 컨텐츠
colnames(raw_review_strength) = c("id","content","brand_nm")
raw_review_strength$content = gsub(",","",raw_review_strength$content)
raw_review_strength$content = gsub("\"","'",raw_review_strength$content)
raw_review_strength$content = stri_replace_all_regex(raw_review_strength$content, pattern = "[\x80-\xFF]", replacement = "")

#단점 컨텐츠
colnames(raw_review_weakness) = c("id","content","brand_nm")
raw_review_weakness$content = gsub(",","",raw_review_weakness$content)
raw_review_weakness$content = gsub("\"","'",raw_review_weakness$content)
raw_review_weakness$content = stri_replace_all_regex(raw_review_weakness$content, pattern = "[\x80-\xFF]", replacement = "")

#기타 컨텐츠
colnames(raw_review_etc) = c("id","content","brand_nm")
raw_review_etc$content = gsub(",","",raw_review_etc$content)
raw_review_etc$content = gsub("\"","'",raw_review_etc$content)
raw_review_etc$content = stri_replace_all_regex(raw_review_etc$content, pattern = "[\x80-\xFF]", replacement = "")

# 리뷰 합친것
raw_review_all = raw_review_CMC %>% dplyr::select(review_no, review_strength, review_weakness, review_etc,brand_nm)
raw_review_all$review_etc[is.na(raw_review_all$review_etc)] = ""
raw_review_all = raw_review_all %>% mutate(review_all = paste(review_strength, review_weakness, review_etc, sep = " "))

raw_review_all = raw_review_all %>% select(review_no, review_all,brand_nm)
colnames(raw_review_all) = c("id","content","brand_nm")
raw_review_all$content = gsub(",","",raw_review_all$content)
raw_review_all$content = gsub("\"","'",raw_review_all$content)
raw_review_all$content = stri_replace_all_regex(raw_review_all$content, pattern = "[\x80-\xFF]", replacement = "")



# 데이터 저장
save.image("./backup/forReviewAnalysis.RData")
gc()
