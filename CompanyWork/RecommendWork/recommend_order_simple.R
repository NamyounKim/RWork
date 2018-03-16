library(recommenderlab)
library(stringi)
library(reshape2)
library(arules)
library(data.table)
library(dplyr)

source("./recommendFunction.R") # 함수불러오기

order_product = merge(order_product, product_marster %>% select(ap_prd_cd,onl_prd_nm,prd_ltyp_nm_n), by.x="v_productcd", by.y = "ap_prd_cd", all.x=T)


#P999 : AP유료 멤버쉽 구매
#P010, P023, P025 P022 만 사용
order_raw = order_product %>% filter(v_product_typecd %in% c("P010", "P023", "P025", "P022")) %>% select(v_ordercd, v_productcd)


# 주문ID, 상품ID 별로 집계하기
grpCols = lapply(c("v_ordercd","v_productcd"), as.symbol)  ## Group by Target column 정하기 
product_order_agg = makeSubDataSet(order_raw, grpCols,"")
product_order_agg = data.table(product_order_agg)

# 주문ID X 상품ID 매트릭스 생성
opvm = dcast.data.table(product_order_agg, v_ordercd ~ v_productcd, sum, value.var = "pv")


# Convert to Rating Matrix & Get Similarity (Product Vector, 최소 상품당 PV, 최소 세션별 PV)
product_order_rm =  makeRatingMatrix(opvm, keyCol = "v_ordercd", colLimit = 2, rowLimit = 2)

## Recommender Modeling ##
recoModel_order = Recommender(product_order_rm, method="IBCF", param=list(method="Jaccard", alpha = 0.1))
saveRDS(recoModel_order, "./recoModel_order.RDS")


## Print Item Simility ##
itemMat = as(getModel(recoModel_order)$sim, "matrix")
itemMatDf = as.data.frame(itemMat)
itemMatDf$pid = rownames(itemMatDf)
itemSim = melt(itemMatDf, id=c("pid"))
itemSim = subset(itemSim, itemSim$value > 0)

# 내림차순으로 보기
itemSim = itemSim %>% arrange(pid, -value)


