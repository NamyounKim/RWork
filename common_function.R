library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(rjson)
library(reshape2)
library(stringi)
library(lubridate)
library(readr)

#차트 테마
th = theme(axis.text = element_text(family = "AppleGothic")
           , legend.text = element_text(family = "AppleGothic")
           , axis.text.x=element_text(angle = 45, hjust = 1)
           , plot.title = element_text(hjust = 0.5, face = "bold"))

th2 = theme(axis.text = element_text(family = "AppleGothic")
           , legend.text = element_text(family = "AppleGothic")
           , plot.title = element_text(hjust = 0.5, face = "bold"))

# Here we define spaces as the big separator
point = format_format(big.mark = ",", scientific = FALSE)

#연령대 다시 만들기
make_new_age_band <- function(input_data){
  
  input_data = input_data %>% mutate(new_age_band = case_when(input_data$age_band %in% c(15) ~ "10s"
                                                            ,input_data$age_band %in% c(20,24,27) ~ "20s"
                                                            ,input_data$age_band %in% c(30,34,37) ~ "30s"
                                                            ,input_data$age_band %in% c(40,44,47) ~ "40s"
                                                            ,input_data$age_band %in% c(50,54,57) ~ "50s"
                                                            ,input_data$age_band %in% c(60,64,67) ~ "60s"))
  input_data = as.data.table(input_data)
  return(input_data)
}

#날짜관련 정보 생성 (YYYY-MM-DD 형식의 날짜가 있을 경우)
make_date_info <- function(input_data = as.data.frame(), date_col_name= as.character(), season_yn = T){
  
  input_data = as.data.frame(input_data)
  
  #시간정보 제외
  input_data$date_id = substr(input_data[,date_col_name], 1,10)
  
  #년도 만들기
  input_data$year_id = substr(input_data[,date_col_name], 1,4)
  
  #년도월 만들기
  input_data$month_id = substr(input_data[,date_col_name], 1,7)
  
  #요일 만들기
  input_data$week_day = wday(as.Date.character(input_data[,date_col_name]), label = T)
  
  if(season_yn == T){
    #계절만들기
    input_data = input_data %>% mutate(season = case_when(substr(input_data[,date_col_name], 6,7) %in% c("12","01","02") ~ "4.Winter"
                                                          ,substr(input_data[,date_col_name], 6,7) %in% c("03","04","05") ~ "1.Spring"
                                                          ,substr(input_data[,date_col_name], 6,7) %in% c("06","07","08") ~ "2.Summer"
                                                          ,substr(input_data[,date_col_name], 6,7) %in% c("09","10","11") ~ "3.Autumn"))
  }
  
  return(input_data)
}


qcProgress <- function(input_dt, ouput_file_name){
  
  mode_val = apply(input_dt, 2, mode)
  
  # Numeric 변수 체크
  numeric_column = as.numeric(as.vector(which(mode_val == "numeric")))
  target_numeric = input_dt[,..numeric_column]
  
  
  # Numeric 변수가 있는 경우에서 실행
  if(nrow(target_numeric) > 0){
    max_val = apply(target_numeric, 2, function(y) max(y, na.rm=TRUE))
    min_val = apply(target_numeric, 2, function(y) min(y, na.rm=TRUE))
    mean_val = apply(target_numeric, 2, function(y) mean(y, na.rm=TRUE))
    sd_val = apply(target_numeric, 2, function(y) sd(y, na.rm=TRUE))
    median_val = apply(target_numeric, 2, function(y) median(y, na.rm=TRUE))
    percentile_val = apply(target_numeric, 2, function(y) quantile(y, prob = seq(0,1, by=0.1), na.rm=TRUE))
    percentile_val = t(percentile_val)
    null_n = apply(target_numeric, 2, function(y) sum(length(which(is.na(y)))))
    null_ratio = null_n / nrow(target_numeric)
    
    qc_output_numeric = data.frame(min_val, max_val, mean_val, sd_val, median_val, null_n, null_ratio)
    qc_output_numeric = cbind(qc_output_numeric, percentile_val)
    write_csv(qc_output_numeric, path = paste("./",ouput_file_name,".csv"))
  }else{
    qc_output_numeric = NULL
  }
  
  ## String 변수 체크
  character_column = as.numeric(as.vector(which(mode_val == "character")))
  target_character = input_dt[,..character_column]
  
  
  # String 변수가 있는 경우에서 실행
  if(nrow(target_character) > 0 ){
    # 각 컬럼별 NULL값 개수
    null_char_n = apply(target_character, 2, function(y) sum(length(which(is.na(y)))))
    # 각 컬럼별 NULL값 비중
    null_char_ratio = null_char_n / nrow(target_character)
    # 각 컬럼별 범주 개수
    code_char_n = apply(target_character, 2, function(y) length(tapply(y,y,length)))
    
    qc_output_character = data.frame(null_char_n, null_char_ratio, code_char_n)
    write_csv(qc_output_character, path = paste("./",ouput_file_name,".csv"))
  }else{
    qc_output_character = NULL
  }
  
  ## 논리형(T,F) 변수 체크
  logical_column = as.numeric(as.vector(which(mode_val == "logical")))
  target_logical = input_dt[,..logical_column]
  
  if(ncol(target_logical) != 0){
    # 각 컬럼별 NULL값 개수
    null_logical_n = apply(target_logical, 2, function(y) sum(length(which(is.na(y)))))
    # 각 컬럼별 NULL값 비중
    null_logical_ratio = null_logical_n / nrow(target_logical)
    # 각 컬럼별 범주별 개수
    code_logical_n = apply(target_logical, 2, function(y) length(tapply(y,y,length)))
    
    qc_output_logical = data.frame(null_logical_n, null_logical_ratio, code_logical_n)
    write_csv(qc_output_logical, path = paste("./",ouput_file_name,".csv"))
    
  }else{
    qc_output_logical = NULL
  }
  
  return(list(qc_output_numeric, qc_output_character, qc_output_logical))
         
}


col_processing <- function(input_dt, target_char, change_char){
  col_n = ncol(input_dt)
  
  for(i in 1:col_n){
    print(i)
    target_row_index = as.numeric(which(input_dt[,..i] == target_char))
    print(target_row_index)
    if(length(target_row_index) > 0){
      input_dt[target_row_index,i] = change_char
    }
  }
  return(input_dt)
}

