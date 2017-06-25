
#Load Data (본인의 파일 경로명을 입력해준다)
#train은 학습용 데이터
train <- read.csv(file="./2_bike/train.csv")

#test는 모델 예측 평가 데이터
test <- read.csv(file="./2_bike/test.csv")

#Data Summary
summary(train)

#상위 10개 Data 확인
head(train,10)
head(test,10)

#변수명 확인
colnames(train)
colnames(test)

#특정 변수(season)에 접근하여 조회하기
head(train$season,7)

#기온 평균값 구하기
mean(train$temp)

train_season <- aggregate(count ~ season, train, sum)
barplot(train_season$count, names.arg=c('spring','summer','fall','winter'), xlab='season', ylab='number of total rentals')

train$year <- as.POSIXlt(train$datetime)$year+1900
boxplot(count ~ year, train, xlab='Year', ylab='number of total rentals')
boxplot(count ~ hour, train, xlab='Hour', ylab='number of total rentals')
#문제 (할 수 있는 문제만 풀면 됩니다.)
# 1. 습도의 평균, 최대값, 최소값 구하기
# 2. 2011년 7월 평균 기온 구하기
# 3. 2012년 9월 평균 렌트 횟수 구하기
# 4. 시간대별 평균 렌트 횟수 구하기
# 5. 주말, 평일, 공휴일을 나누는 변수 새로 만들기
# 6. 계절별 렌트횟수 구하기 (group by season)
# 7. 위에서 구한 값으로 계절별 렌트횟수 막대차트 그리기
# 8. 렌트횟수와 기온간의 상관계수값 구하기
# 9. 년도별/시간대별 렌트횟수 boxplot 그리기





