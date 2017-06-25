install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
library(MASS)

data("Cars93")
Cars93

##1. Bar Chart
ggplot(Cars93, aes(x=Type)) + geom_bar(stat = "count")
ggplot(Cars93, aes(x=Type)) + geom_bar(stat = "count") + labs(x="Car Type") + theme_minimal()
ggplot(Cars93, aes(x=Type)) + geom_bar(stat = "count") + labs(x="Car Type") + 
  ggtitle("자동차 타입별 건수") + theme(plot.title = element_text(hjust = 0.1, size = 25)) # 차트에 제목 넣기

##2. Stack Bar Chart
ggplot(Cars93, aes(x=Type, fill=Origin)) + geom_bar(stat = "count")
ggplot(Cars93, aes(x=Type, fill=Origin)) + geom_bar(stat = "count", position = "dodge") # Stack 하지 않기

#type별 가격 평균값 구한 후 바차트 그리기
mpg_temp = Cars93 %>% group_by(Type) %>% summarise(avgPrice = mean(Price))
ggplot(mpg_temp, aes(x=Type, y=avgPrice)) + geom_bar(stat = "identity") #데이터프레임 내 값 그대로

#비율 누적 Bar Chart 구하기
#자동차 type별 Origin 비율구한 후 비율 누적 Bar Chart 그리기
df2 = Cars93 %>% group_by(Type, Origin) %>% summarise(n=n()) %>% 
  mutate(ratio = n/sum(n))

ggplot(df2, aes(x=Type, y=ratio, fill=Origin)) + geom_bar(stat = "identity")


##3. Scatter Plot
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point(stat = "identity") 
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point(shape=3) # 점모양 바꾸기
ggplot(Cars93, aes(x=MPG.city, y=Price, label = Manufacturer)) + geom_text(size=3) # 점 대신 라벨값 보여주
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point(shape=2) + geom_smooth(method=lm) # 선형식 그리기
ggplot(Cars93, aes(x=MPG.city, y=Price)) + geom_point(shape=2) + geom_smooth() #다항식 그리기
ggplot(Cars93, aes(x=MPG.city, y=Price, color=factor(Manufacturer))) + geom_point(shape=2) #특정 범주형 값 별로 색깔 다르게 하기
ggplot(Cars93, aes(x=MPG.city, y=Price, color=factor(Manufacturer), size=EngineSize)) + geom_point(shape=18) #특정 범주형 값 별로 색깔/크기 다르게 하기

##4. Box Plot
ggplot(Cars93, aes(x=factor(Manufacturer), y=Price)) + geom_boxplot() + theme(axis.text.x=element_text(angle = 45, hjust = 1))
ggplot(Cars93, aes(x=factor(Type), y=Price, fill=factor(Origin))) + geom_boxplot()

#샘플 데이터 가져오기
data("economics")
economics

##5. Line Chart
ggplot(economics, aes(x=date, y=pop)) + geom_line(stat = "identity")

ggplot(economics, aes(x=pop, y=unemploy)) + geom_point()

#축 눈금 간격 조절하기
ggplot(economics, aes(x=pop, y=unemploy)) + geom_point() + 
  scale_x_continuous(breaks = seq(200000, max(economics$pop), 10000)) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

economics$year = substr(economics$date,1,4)
ggplot(economics, aes(x=pop, y=unemploy, color=factor(year))) + geom_point()


##6. 상관계수행렬 그리기
install.packages("corrplot")
library(corrplot)
subCars93 = Cars93 %>% dpselect(Price, MPG.city, MPG.highway, EngineSize)
plot(subCars93)

#상관계수 구하기
corDf = cor(subCars93)

corrplot(corDf)
corrplot(corDf, method = "shade", tl.srt=45)
corrplot(corDf, method = "shade", tl.srt=45, addCoef.col = "black", order="AOE")
corrplot(corDf, method = "shade", tl.srt=45, addCoef.col = "black", order="AOE", type = "lower")
