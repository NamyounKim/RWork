library(recommenderlab)
library(stringi)
library(reshape2)
library(arules)
library(data.table)
library(dplyr)

source("./recommendFunction.R")

reco_raw = raw_log %>% filter(!(cur_pg_nm %in% c("웹 주문결제","웹 주문완료","웹 장바구니","모바일 주문결제","모바일 주문완료","모바일 장바구니"))) %>%
  filter(prd_list != "")

#Make PK by session
reco_raw$sid = paste0(trimws(reco_raw$omni_id), "_", trimws(reco_raw$comcsno_w), "_", trimws(reco_raw$omni_visit_seq), "_",trimws(stri_sub(reco_raw$conn_dttm, 7,8)))

# Get PID FROM URL
reco_raw$prePid = stri_match_first_regex(reco_raw$saint_cd, pattern = "sProductcd=[A-Za-z0-9]+")
reco_raw$v_productcd = stri_replace_all_fixed(reco_raw$prePid,"sProductcd=","")

# Remove the row have no pid
reco_raw = reco_raw[!is.na(reco_raw$v_productcd),]

product_pv = subset(reco_raw, select=c(sid, v_productcd))


# SID별 집계 By specific column
grpCols = lapply(c("sid","v_productcd"), as.symbol)  ## Group by Target column 정하기 

product_pv_agg =makeSubDataSet(product_pv, grpCols,"")

product_pv_agg = data.table(product_pv_agg)

start = Sys.time()
product_pv_spvm = dcast.data.table(product_pv_agg, sid ~ v_productcd, sum, value.var = "pv")
end = Sys.time()
end-start

start = Sys.time()
# Convert to Rating Matrix & Get Similarity (Product Vector, 최소 상품당 PV, 최소 세션별 PV)
product_pv_rm =  makeRatingMatrix(product_pv_spvm, keyCol = "sid", colLimit = 50, rowLimit = 4)

## Recommender Modeling ##
recoModel = Recommender(product_pv_rm, method="IBCF", param=list(method="Jaccard", alpha = 0.1))
saveRDS(recoModel, "./recoModel.RDS")
end = Sys.time()
end-start


## Print Item Simility ##
itemMat = as(getModel(recoModel)$sim, "matrix")
itemMatDf = as.data.frame(itemMat)
itemMatDf$brandNm = rownames(itemMatDf)
itemSim = melt(itemMatDf, id=c("pid"))
itemSim = subset(itemSim, itemSim$value > 0)
saveRDS(itemMatDf,"./itemMatDf.RDS")

itemMatDf %>% select(brandNm, 해피바스) %>% arrange(-해피바스)



