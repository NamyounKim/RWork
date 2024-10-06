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