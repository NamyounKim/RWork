# 문제1. 엔진사이즈(EngineSize)가 3.0 이상이고, 유형(Type)이 "Large"인 행만 추출하기
subsetss = Cars93 %>% filter(EngineSize > 3.0 & Type == "Large")

# Manufacturer(제조사) 별로 자동차 개수를 바차트로 그리기
ggplot(subsetss, aes(x= Manufacturer)) + geom_bar(stat = "count")