library(rjson)
library(httr)
library(curl)
library(readxl)

APP_KEY = "PS5BgtA0hnoZGzfNGwGM3Tdx66FTIb4JgE1L"
APP_SECRET = "cR5mshzPRRJE2QFhBXY7JkMxkyVIk6v2MSxlidIAYxxhSwiUPA5Sh4dAHurnvtlnSFgg/dU7+ag5f4lqhMLLpJgZj5UcZKmSasKwMeM6b4SFZ2l3BgpxMVUSeDwkE+06iC01LmPgdnMJuJyCT9j9xdDyH5Mz529leCOOlS6pCikuwDALus4="
URL_BASE = "https://openapi.koreainvestment.com:9443" #해외투자 주문

##-- access token 얻기 -------------------------
PATH = "/oauth2/tokenP"
URL = paste0(URL_BASE, "/", PATH)

body_json <- list(
  grant_type = "client_credentials"
  ,appkey = APP_KEY
  ,appsecret = APP_SECRET
)

res <- POST(URL
            ,body = body_json
            ,encode = "json")

res_data = content(res)

access_token = res_data$access_token

##-- 잔액 조회 하기 -------------------------------------------------------------------------------------------------------------------------------
path_balance = "/uapi/overseas-stock/v1/trading/inquire-balance"
#URL = paste0(URL_BASE, path_balance)

query_json = list(
  CANO = "72710315"
  ,ACNT_PRDT_CD = "01"
  ,OVRS_EXCG_CD = "NASD"
  ,TR_CRCY_CD = "USD"
  ,CTX_AREA_FK200 = ""
  ,CTX_AREA_NK200 = ""
)

res2 = GET(url = URL_BASE
           ,path = path_balance
           ,add_headers(grant_type = "client_credentials"
                        ,authorization = paste("Bearer", access_token)
                        ,appkey = APP_KEY
                        ,appsecret = APP_SECRET
                        ,tr_id = "TTTS3012R")
          ,query = query_json
)
content(res2)$msg1
res_balance_data = content(res2)

balance_output1 = res_balance_data$output1
etf_aim_balance = do.call(rbind.data.frame, balance_output1)
etf_aim_balance = as.data.table(etf_aim_balance)

etf_aim_balance$pchs_avg_pric = as.integer(etf_aim_balance$pchs_avg_pric)
etf_aim_balance$ovrs_cblc_qty = as.integer(etf_aim_balance$ovrs_cblc_qty)
etf_aim_balance$frcr_evlu_pfls_amt = as.integer(etf_aim_balance$frcr_evlu_pfls_amt)
etf_aim_balance$frcr_pchs_amt1 = as.integer(etf_aim_balance$frcr_pchs_amt1)
etf_aim_balance$ovrs_stck_evlu_amt = as.integer(etf_aim_balance$ovrs_stck_evlu_amt)

etf_aim_balance = etf_aim_balance %>% mutate(evlu_ratio = percent(frcr_pchs_amt1 / sum(frcr_pchs_amt1))
                                             ,stck_ratio = percent(ovrs_stck_evlu_amt / sum(ovrs_stck_evlu_amt)))

sum(etf_aim_balance$frcr_evlu_pfls_amt)

etf_aim_balance[,.(ovrs_pdno, ovrs_item_name, evlu_ratio, stck_ratio)]
etf_aim_balance_230623[,.(ovrs_pdno, ovrs_item_name, evlu_ratio, stck_ratio)]

saveRDS(etf_aim_balance, paste0("./data/etf_aim_balance_", stri_replace_all_fixed(Sys.Date(), "-", ""), ".rds"))

##-- 체결 기준 현재 잔고 조회 하기 -------------------------------------------------------------------------------------------------------------------------------
path_present_balance = "/uapi/overseas-stock/v1/trading/inquire-present-balance"

query_json = list(
  CANO = "72710315"
  ,ACNT_PRDT_CD = "01" #계좌상품코드
  ,WCRC_FRCR_DVSN_CD = "01" #원화외화구분코드
  ,NATN_CD = "840" #국가코드
  ,TR_MKET_CD = "00" #거래시장코드
  ,INQR_DVSN_CD = "01"  #조회구분코드

)

res2 = GET(url = URL_BASE
           ,path = path_balance
           ,add_headers(grant_type = "client_credentials"
                        ,authorization = paste("Bearer", access_token)
                        ,appkey = APP_KEY
                        ,appsecret = APP_SECRET
                        ,tr_id = "CTRP6504R")
           ,query = query_json)

content(res2)$msg1
res_present_balance_data = content(res2)

present_balance_output1 = res_present_balance_data$output1
etf_aim_present_balance = do.call(rbind.data.frame, present_balance_output1)
etf_aim_present_balance = as.data.table(etf_aim_present_balance)

etf_aim_present_balance2 = as.data.table(apply(etf_aim_present_balance[,2:10], MARGIN = 2, FUN = as.double))
etf_aim_present_balance[,2:10] = etf_aim_present_balance2
etf_aim_present_balance$ovrs_now_pric1 = as.numeric(etf_aim_present_balance$ovrs_now_pric1)
etf_aim_present_balance$avg_unpr3 = as.numeric(etf_aim_present_balance$avg_unpr3)

etf_aim_present_balance = etf_aim_present_balance %>% mutate(pchs_won_ratio = percent(frcr_pchs_amt / sum(frcr_pchs_amt))
                                                             ,evlu_won_ratio = percent(frcr_evlu_amt2 / sum(frcr_evlu_amt2)))

#평가금액 합계
sum(etf_aim_present_balance$frcr_evlu_amt2)

#손익 합계
sum(etf_aim_present_balance$evlu_pfls_amt2)

saveRDS(etf_aim_present_balance, paste0("./data/etf_aim_present_balance_", stri_replace_all_fixed(Sys.Date(), "-", ""), ".rds"))
