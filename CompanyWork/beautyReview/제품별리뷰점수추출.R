# 점수가 0인 경우 제외하고 평균 구하기
mean_except_zero = function(x){
  if(length(which(x>0)) == 0){
    return(0)
  }else{
    return(mean(x[which(x>0)], na.rm = T))
  }
}

# Upper Outlier를 Upper fence 안 쪽으로 이동 함수
checkUpperFence = function(x){
  upper_fence = quantile(x, 0.75) + 1.5*IQR(x)
  x[x > upper_fence] = upper_fence
  return(x)
}

# 리뷰수가 4개 이상이 상품만 scoring
target_prd = raw_review_strength %>% filter(site_cd=="CMC") %>% group_by(site_product_cd, product_nm) %>% dplyr::summarise(review_n=n()) %>% filter(review_n > 3) %>% select(site_product_cd)

# 제품별 "좋은점" 속성 점수 계산 -----------------------------------------------------------------------------------------------------------------------------
strength_prd_score_apmall = raw_review_strength %>% 
  filter(site_cd=="CMC", site_product_cd %in% target_prd$site_product_cd) %>% 
  group_by(site_product_cd, product_nm) %>% 
  dplyr::summarise(review_n=n()
                  ,발색 = mean_except_zero(a1)
                  ,커버력= mean_except_zero(a2)
                  ,밀착력_지속력= mean_except_zero(a3)
                  ,세정력= mean_except_zero(a4)
                  ,향= mean_except_zero(a5)
                  ,흡수력= mean_except_zero(a6)
                  ,보습력= mean_except_zero(a7)
                  ,안티에이징_미백= mean_except_zero(a8)
                  ,천연= mean_except_zero(a9)
                  ,휴대성= mean_except_zero(a10)
                  ,가성비= mean_except_zero(a11)
                  ,사용편리성= mean_except_zero(a12)) %>% 
  arrange(-review_n)

# Outlier를 upper fence 안쪽으로 이동
byColScale = apply(strength_prd_score_apmall[,4:15], 2, checkUpperFence)

# 열별로 0 to 1로 rescale
byColScale = apply(byColScale, 2, function(x){(x-min(x))/(max(x)-min(x))})
byColScale = round(byColScale * 5, digits = 1)
strength_prd_score_apmall_col = cbind.data.frame(strength_prd_score_apmall[,1:3], byColScale)

write_csv(strength_prd_score_apmall_col, "./좋은점_제품별_속성점수.csv")



# 제품별 "아쉬운점" 속성 점수 계산 -----------------------------------------------------------------------------------------------------------------------------
weakness_prd_score_apmall = raw_review_weakness %>% 
  filter(site_cd=="CMC", site_product_cd %in% target_prd$site_product_cd) %>% 
  group_by(site_product_cd, product_nm) %>% 
  dplyr::summarise(review_n=n()
                   ,발색 = mean_except_zero(a1)
                   ,커버력= mean_except_zero(a2)
                   ,밀착력_지속력= mean_except_zero(a3)
                   ,세정력= mean_except_zero(a4)
                   ,향= mean_except_zero(a5)
                   ,흡수력= mean_except_zero(a6)
                   ,보습력= mean_except_zero(a7)
                   ,안티에이징_미백= mean_except_zero(a8)
                   ,천연= mean_except_zero(a9)
                   ,휴대성= mean_except_zero(a10)
                   ,가성비= mean_except_zero(a11)
                   ,사용편리성= mean_except_zero(a12)) %>% 
  arrange(-review_n)

# Outlier를 upper fence 안쪽으로 이동
weakness_byColScale = apply(weakness_prd_score_apmall[,4:15], 2, checkUpperFence)

# 열별로 0 to 1로 rescale
weakness_byColScale = apply(weakness_byColScale, 2, function(x){(x-min(x))/(max(x)-min(x))})
weakness_byColScale = round(weakness_byColScale * 5, digits = 1)
weakness_prd_score_apmall_col = cbind.data.frame(weakness_prd_score_apmall[,1:3], weakness_byColScale)


