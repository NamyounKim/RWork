
## 7. 문자열 핸들링 ----------------------------------------------------------------------------------------------------------------------------------------------------------------
sentence = c("I like an apple.")

#매칭여부 확인하기
grepl("like", sentence)

#매칭되는 Vector index값 구하기
grep("like", sentence)
grep(pattern = "like", x = sentence)
sentence2 = c("I", "like", "an", "apple.")
grep("like", sentence2)

#매칭되는 문자열 바꾸기
gsub("like", "hate", sentence)

#문자열 붙이기
paste(sentence, "And I have the apple.")

#특정 위치 문자열 가져오기
substr(sentence, 4, 7)
substr(x = sentence, start = 4, stop = 7)
substr(sentence, 1, 6) = "I hate" # 1번째 문자에서 6문자 사이의 문자열 바꾸기

#문자열 내에 해당 패턴이 나오는 첫번째 위치 구하기
regexpr("hate",sentence)

#문자열 내에 해당 패턴이 나오는 모든 위치 구하기
gregexpr("a", sentence)

#문자열 구분자에 따라 나누기
split = strsplit(sentence, " ")
split = unlist(split) # Vector 형식으로 바꿔주기



## 8. dplyr패키지로 데이터 핸들링하기 -----------------------------------------------------------------------------------------------------------------------------------------------------

install.packages("dplyr")
library(dplyr)
library(MASS)

# Cars93이라는 데이터 가져오기
data(Cars93)
Cars93

# 8-1. 특정 column 선택하기
select(.data = Cars93,  Manufacturer, Model, Type, Price)

dSample = dplyr::select(Cars93, Manufacturer, Model, Type, Price)
head(dSample, 5)

# 8-2. 특정 row 선택하기
filter(.data = dSample, Price > 30)
filter(dSample, Price > 30)
filter(dSample, Price > 30 & Manufacturer  =="Audi")
filter(dSample, Type %in% c("Compact", "Van","Small","Midsize"))

# 8-3. Sorting 하기
arrange(dSample, -Price) #내림차순

# 8-4. 파이프 연산자를 이용하여 Group by 하기
dSampleBy = dSample %>% group_by(Manufacturer, Type) %>% summarise(mean_price = mean(Price))
                                   
# 8-5. 집계 컬럼 2개 동시에 만들기
dSampleBy = dSample %>% group_by(Manufacturer, Type) %>% summarise(mean_price = mean(Price), car_n = n())

# 8-6. 새로운 column 만들기(mutate)
dSampleMutate = dSample %>% group_by(Manufacturer) %>% summarise(total_price = sum(Price), car_n = n()) %>% mutate(mean_price = total_price / car_n)

# 8-7. 파이프 연산자만 사용하기
dSampleAll = Cars93 %>% dplyr::select(Manufacturer, Model, Type, Price) %>% filter(Price > 30) %>% group_by(Manufacturer) %>% summarise(mean_price = mean(Price)) %>% arrange(desc(mean_price))


## 9. reshape2 패키지로 데이터 변형하기 -----------------------------------------------------------------------------------------------------------------------------------------------------

install.packages("reshape2")
library(reshape2)

# 9-1. dcast 함수
castTest = dcast(data = Cars93
                 , formula = Model ~ Type
                 , value.var = "Min.Price"
                 , fill= 0
                 , margins = FALSE)

# 9-2. melt 함수
meltTest = melt(data = Cars93, 
                 id.vars = "Type", 
                 measure.vars = c("Min.Price", "Max.Price"))




## 9. 제어, 반복문 -------------------------------------------------------------------------------------------------------------------------------------------------------------

a = c(1,8,5)

# 9-1. if문
if(length(a) > 1){
  mean(a)
}

# 9-2. else 문
if(length(a) > 5){ 
  mean(a)
} else {
  print("조건에 맞지 않습니다.")
}


# 9-3. ifelse문
ifelse(length(a) > 5, mean(a), "조건에 맞지 않습니다.")


# 9-4.for문 (ex. 0부터 10까지 더하기)
sum=0
for (i in 1:10){
  sum = sum + i
}
sum


# 9-5.while 문 (ex. 3의 배수 출력하기)
i = 1
while(i<=10){
  print(i*3)
  i = i+1
}


# 9-6.Function 만들기
myFunction = function(data, company){
  temp = subset(data, data$Manufacturer == company)
  sumPrice = sum(temp$Price)
  
  return(sumPrice)
}

aa = myFunction(Cars93, "Audi")
aa

