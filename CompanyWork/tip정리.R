#====== 1.멀티 프로세싱 방법 ======#
library(parallel)
processingFun = function(i){
  # mclapply에서 사용할 함수 정의
  # 이때 mclapply에서 사용한 iter_index가 파라메터로 사용된다.
  # 그리고 함수 안에서 사용되는 데이터는 만들어져 있는 것을 사용한다.
  
  return(targetTemp2)
}

sample_result = mclapply(1:nrow(sample), processingFun, mc.cores = 10)
  # sample의 길이 만큼 반복 하면서 processingFun 함수를 호출하는데 cpu 10개를 할당한다.


#====== 2. 맵핑표가 있을 경우 동의어/유사값 처리 ======#
for (i in 1:nrow(synonymDic)){
  # replace하고자 하는 원단어 위치값 구하기
  targetDocIdx = which(ll <- grepl(synonymDic$originWord[i], parsedText))
  
  # 만약 위치값이 있을 경우 해당 위치값의 원문 내용을 순서대로 replace하기
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    parsedText[docNum] = gsub(synonymDic$originWord[i], synonymDic$changeWord[i], parsedText[docNum])
  }
}


#====== 3. String을 날짜 형식으로 전환 (YYYY-MM-DD ======#

library(stringi)
convertDate = function(obj){
  temp =paste0(substr(obj, 1,4),"-",
               substr(obj, 5,6),"-",
               substr(obj, 7,8))
  temp[temp == "NA-NA-NA"] = NA
  return(stringi::stri_datetime_format(time = as.POSIXct(temp), format = "uuuu-MM-dd"))
}
convertDate(target_ctr$lastPurDt)



#====== 4. Convert 인코딩 ======#
convertEncoding = function(target){
  for(i in 1:ncol(target)){
    # 컬럼타입이 character인 컬럼만 catch
    if(mode(target[,i]) == "character"){
      target[,i] = stri_conv(target[,i], from="EUC-KR", to="UTF-8")
    }
  }
  
  return(target)
}
mart_last_product_view_apmall = convertEncoding(mart_last_product_view_apmall)


#====== 5. 테이블 QC 함수 ======#
qcProgress = function(target, index){
  
  mode_val = NULL
  for(i in 1:ncol(target)){
    mode_val[i] = mode(target[,i])
  }
  
  # Numeric 변수 체크
  target_numeric = target[,mode_val=="numeric"]
  
  max_val = apply(target_numeric, 2, function(y) max(y, na.rm=TRUE))
  min_val = apply(target_numeric, 2, function(y) min(y, na.rm=TRUE))
  mean_val = apply(target_numeric, 2, function(y) mean(y, na.rm=TRUE))
  sd_val = apply(target_numeric, 2, function(y) sd(y, na.rm=TRUE))
  median_val = apply(target_numeric, 2, function(y) median(y, na.rm=TRUE))
  percentile_val = apply(target_numeric, 2, function(y) quantile(y, prob = seq(0,1, by=0.1), na.rm=TRUE))
  null_n = apply(target_numeric, 2, function(y) sum(length(which(is.na(y)))))
  null_ratio = null_n / nrow(target_numeric)

  qc_output_numeric = data.frame(min_val_df, max_val_df, mean_val_df, sd_val_df, median_val_df, null_n_df, null_ratio_df, percentile_val_df)
  write.csv(qc_output_numeric, file = paste("./qc_output_numeric_",index,".csv"))
  
  ## String 변수 체크
  target_character = target[,mode_val=="character"]
  
  # 각 컬럼별 NULL값 개수
  null_char_n = apply(target_character, 2, function(y) sum(length(which(is.na(y)))))
  # 각 컬럼별 NULL값 비중
  null_char_ratio = null_char_n / nrow(target_character)
  # 각 컬럼별 범주별 개수
  code_char_n = apply(target_character, 2, function(y) length(tapply(y,y,length)))
  
  ## 논리형(T,F) 변수 체크
  target_logical = target[,mode_val=="logical"]
  # 각 컬럼별 NULL값 개수
  null_logical_n = apply(target_logical, 2, function(y) sum(length(which(is.na(y)))))
  # 각 컬럼별 NULL값 비중
  null_logical_ratio = null_logical_n / nrow(target_logical)
  # 각 컬럼별 범주별 개수
  code_logical_n = apply(target_logical, 2, function(y) length(tapply(y,y,length)))
  
  
  qc_output_string1 = data.frame(null_char_n_df, null_char_ratio_df, code_char_n_df)
  write.csv(qc_output_string1, file = paste("./qc_output_string1_",index,".csv"))
  
  qc_output_string2 = data.frame(null_logical_n_df, null_logical_ratio_df, code_logical_n_df)
  write.csv(qc_output_string2, file = paste("./qc_output_string2_",index,".csv"))
  
  return(qc_output_numeric)
}
