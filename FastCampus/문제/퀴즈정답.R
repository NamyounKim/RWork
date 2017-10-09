library(dplyr)
library(MASS)
data("Cars93")
Cars93

#문제1. 엔진사이즈(EngineSize)가 3.0 이상이고, 유형(Type)이 "Large"인 행만 추출하기
q1 = Cars93 %>% filter(EngineSize > 3.0 & Type == "Large")
subset(Cars93, (Cars93$EngineSize > 3.0) & (Cars93$Type == "Large"))

#문제2. 위에서 만든 데이터에서 제조사(Manufacture), 모델(Model), 가격(Price) 열만 추출하기
q2 = q1 %>% dplyr::select(Manufacturer, Model, Price)

#문제3. 2번에서 만든 데이터의 컬럼명을 "AA", "BB", "CC" 로 변경하기
colnames(q2) = c("AA","BB","CC")

#문제4. 전체 데이터에서 자동차 에어백(AirBags) 별로 평균 가격 구하기
q4 = Cars93 %>% group_by(AirBags) %>% summarise(avgPrice = mean(Price))

#문제5. for문을 사용해서 1~20 까지의 짝수의 합 구하기.
x = 0
for(i in 1:20){
  if((i %% 2) == 0){
    x = x + i
  }
}
x









