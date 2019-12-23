library(readxl)
library(data.table)
library(dplyr)


#1. 여러개의 대용량 엑셀파일을 하나로 합치기
#2. 입주시기에 따른 하자접수량 비교 (공공데이터 중 입주물량 데이터와 비교하여 전국 입주물량이 많을 경우 기능공 수급이 어려워져 품질이 나빠진다는 가정을 확인)
#3. 입주후 시기별 하자접수량 (입주 초기에 하자접수가 급격하게 증가하다가 어느시점부터 얼마나 줄어드는지)
#4. 현장명을 임의로 지역별로 나눈 후, 지역별 하자접수 특성, 하자처리 특성 등
#5. 하자접수 텍스트를 분석해 '누수' 현장별 누수 하자 분석 등

# read_excel 함수는 엑셀에서 데이터를 읽어올때 쓰는 함수


#file list 가져오기
excel_file_list = list.files("./excel_data/")

raw_data_list = list()

for(i in 1:length(excel_file_list)){
  target_file_path = paste0("./excel_data/", excel_file_list[i])
  print(target_file_path)
  temp = read_excel(target_file_path, sheet = 1, col_names = T)
  raw_data_list[i] = list(temp)
}

raw_data_dt = rbind_list(raw_data_list)

# 엑셀에서 가져온 데이터 저장
saveRDS(raw_data_dt, "./data/raw_data_dt.RDS")



