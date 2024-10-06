
source("./get_api_auth.R")

##-- 특정 주식 시세 조회 하기 -------------------------------------------------------------------------------------------------------------------------------
path_chartprice = "/uapi/domestic-stock/v1/quotations/inquire-daily-itemchartprice"

query_json = list(
  FID_COND_MRKT_DIV_CODE = "J" #시장 분류 코드
  ,FID_INPUT_ISCD = "035720" #종목코드
  ,FID_INPUT_DATE_1 = "20160101" #시작날짜
  ,FID_INPUT_DATE_2 = "20230706"  #종료날짜
  ,FID_PERIOD_DIV_CODE = "D"  #기간분류코드
  ,FID_ORG_ADJ_PRC = "0" #수정주가 원주가 가격 여부
)

request = GET(url = URL_BASE
           ,path = path_chartprice
           ,add_headers(grant_type = "client_credentials"
                        ,authorization = paste("Bearer", access_token)
                        ,appkey = APP_KEY
                        ,appsecret = APP_SECRET
                        ,tr_id = "FHKST03010100")
           ,query = query_json)

#정상처리 여부
content(request)$msg1

#데이터 추출하기
request_content = content(request)

#종목상세
stock_detail = request_content$output1

#종목 일별 데이터
stock_trend = request_content$output2
stock_trend = do.call(rbind.data.frame, stock_trend)
