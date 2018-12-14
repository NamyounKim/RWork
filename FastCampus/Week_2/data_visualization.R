install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(MASS)

#Cars93 데이터 가져오기
data("Cars93")
Cars93

## 1. Bar Chart ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(Cars93, aes(x=Type)) + geom_bar(stat = "count") # 막대의 높이를 빈도수로 할 경우
ggplot(Cars93, aes(x=Type)) + geom_bar(stat = "count") + labs(x="Car Type") # X축에 이름 붙이기
ggplot(Cars93, aes(x=Type)) + geom_bar(stat = "count") + labs(x="Car Type") + 
  ggtitle("The Count by Car Type") + theme(plot.title = element_text(hjust = 0.5, size = 25)) # 차트에 제목 넣기



## 2. Stack Bar Chart ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(Cars93, aes(x=Type, fill=AirBags)) + geom_bar(stat = "count")
ggplot(Cars93, aes(x=Type, fill=AirBags)) + geom_bar(stat = "count", position = "dodge") # Stack 하지 않기

# type별 가격 평균값 구한 후 바차트 그리기
temp_set = Cars93 %>% group_by(Type) %>% summarise(avgPrice = mean(Price))
ggplot(temp_set, aes(x=Type, y=avgPrice)) + geom_bar(stat = "identity") #막대의 높이를 값으로 할 경우

# 비율 누적 Bar Chart 구하기
# 자동차 type별 Origin 비율구한 후 비율 누적 Bar Chart 그리기
temp_set = Cars93 %>% group_by(Type, AirBags) %>% summarise(n=n()) %>% 
  mutate(ratio = n/sum(n))

ggplot(temp_set, aes(x=Type, y=ratio, fill=AirBags)) + geom_bar(stat = "identity")



## 3. Scatter Plot ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point()
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point(shape=9) # 점모양 바꾸기
ggplot(Cars93, aes(x=MPG.city, y=Price, label = Manufacturer)) + geom_text(size=3) # 점 대신 라벨값 보여주기
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point(shape=2) + geom_smooth(method=lm) # 선형식 그리기
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point(shape=2) + geom_smooth() #다항식 그리기
ggplot(Cars93, aes(x=MPG.city, y=Price, color=factor(Manufacturer))) + geom_point(shape=2) #특정 범주형 값 별로 색깔 다르게 하기
ggplot(Cars93, aes(x=MPG.city, y=Price, color=factor(Manufacturer), size=EngineSize)) + geom_point(shape=18) #특정 범주형 값 별로 색깔/크기 다르게 하기



## 4. Box Plot -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(Cars93$Price ~ Cars93$AirBags)
ggplot(Cars93, aes(x=AirBags, y=Price)) + geom_boxplot()
ggplot(Cars93, aes(x=AirBags, y=Price, color=factor(Manufacturer))) + geom_boxplot() + theme(axis.text.x=element_text(angle = 45, hjust = 1)) # x축 텍스트 회전시키기

#샘플 데이터 가져오기
data("economics_long")
economics_long
df = economics_long %>% filter(variable %in% c("psavert", "uempmed")) %>% filter(date >= "1990-01-01") #개인 저축율, 실업이 지속된 기간(주)


## 5. Line Chart -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(df, aes(x=date, y=value)) + geom_line()
ggplot(df, aes(x=date, y=value, color=variable)) + geom_line()

# Line Stack Chart
ggplot(df, aes(x=date, y=value, fill=variable)) + geom_area() + geom_line(position = "stack")

#축 눈금 간격 조절하기
ggplot(df, aes(x=date, y=value, fill=variable)) + geom_area() + geom_line(position = "stack") +
  scale_x_date(date_breaks = "1 year",  date_labels = "%Y") + # X축에 연도 표시하기
  theme(axis.text.x=element_text(angle = 90, hjust = 1))    # X축 텍스트를 45도 기울여서 표시하기



## 6. 상관계수행렬 그리기 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
subCars93 = Cars93 %>% dplyr::select(Price, MPG.city, MPG.highway, EngineSize)
plot(subCars93)

install.packages("psych")
library(psych)
pairs.panels(subCars93)

#상관계수 구하기
corDf = cor(subCars93)
