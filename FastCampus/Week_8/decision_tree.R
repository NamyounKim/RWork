install.packages("C50")
library(MASS)
library(C50)
library(dplyr)

# 데이터 불러오기
data("Cars93")
Cars93

# 예측 변수 유형 확인 하기
tapply(Cars93$Type, Cars93$Type, length)

# 훈련 데이터셋 만들기
trainSet = Cars93 %>% select(-Type)

# 의사결정트리 모델 만들기
fit = C5.0(trainSet, Cars93$Type)

# 결과 확인하기
summary(fit)

# 시각화하기
plot(fit)
