library(readr)
library(data.table)
library(dplyr)

ecommerce_data = read_csv("./2019-Nov.csv", col_names = T
                          ,col_types = list(col_character()
                                            ,col_character()
                                            ,col_character()
                                            ,col_character()
                                            ,col_character()
                                            ,col_character()
                                            ,col_double()
                                            ,col_character()
                                            ,col_character()))
ecommerce_data = as.data.table(ecommerce_data)

#ecommerce_data_oct = as.data.table(ecommerce_data_oct)

#ecommerce_data = rbind(ecommerce_data, ecommerce_data_oct)

stringi::stri_split_fixed(ecommerce_data$category_code[1:100], pattern = ".", simplify = T)

ecommerce_data$date_id = substr(ecommerce_data$event_time, 1, 10)

ecommerce_data[date_id == "2019-11-14"] %>% group_by(event_type) %>% summarise(n= n())

#행수  67,501,979
nrow(ecommerce_data)

#유저수  3,696,117
length(unique(ecommerce_data$user_id))


#브랜드별 건수 및 상품수
brand_dt = ecommerce_data %>% 
  group_by(brand) %>% 
  summarise(row_n = n()
            ,prd_n = n_distinct(product_id))
brand_dt

#카테고리 수 
prd_cate_dt = ecommerce_data %>% 
  group_by(category_code) %>% 
  summarise(row_n = n()
            ,prd_n = n_distinct(product_id))
prd_cate_dt

max(ecommerce_data$event_time)
min(ecommerce_data$event_time)

#브랜드별 장바구니, 구매 CTR
brand_dt = ecommerce_data %>% 
  group_by(brand, event_type) %>% 
  summarise(row_n = n()
            ,user_n = n_distinct(user_id)
            ,session_n = n_distinct(user_session)) %>%
  as.data.table()

brand_pivot = dcast.data.table(brand_dt, brand ~ event_type, fill = 0, value.var = "user_n")
brand_pivot$cart_ctr = brand_pivot$cart / brand_pivot$view
brand_pivot$purchase_ctr = brand_pivot$purchase / brand_pivot$view

brand_session_pivot = dcast.data.table(brand_dt, brand ~ event_type, fill = 0, value.var = "session_n")
brand_session_pivot$cart_ctr = brand_session_pivot$cart / brand_session_pivot$view
brand_session_pivot$purchase_ctr = brand_session_pivot$purchase / brand_session_pivot$view


#카테고리별 유저퍼널
funnel_dt = ecommerce_data %>% 
  group_by(brand, event_type) %>% 
  summarise(row_n = n()
            ,user_n = n_distinct(user_id)
            ,session_n = n_distinct(user_session)) %>%
  as.data.table()
