

# Cars93 데이터셋에서 제조사가 "Hyundai", "Ford" 인 행만 선택하기
subset = Cars93 %>% filter(Manufacturer %in% c("Hyundai", "Ford")) 
cbind(subset$Manufacturer, subset$Model)

# 위 데이터 셋에서 제조사와 모델 column만 남기기
subset %>% dplyr::select(Manufacturer, Model)
