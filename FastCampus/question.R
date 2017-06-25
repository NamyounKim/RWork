#문제1. "hello text world"에서 o를 모두 x로 바꾸기
str = "hello text world"
gsub("o","x",str)

#문제2. "1a2a3a4a5a6" 문자열을 "a"를 구분자로 나눠서 벡터형식으로 저장하기 
str= "1a2a3a4a5a6"
result = strsplit(str, "a")
result = unlist(result)
result

#문제3. dcast함수를 통해 Hyundai, Chevrolet, Audi, Toyota의 평균 가격을 TYPE별로 비교할 수 있는 데이터프레임 만들기
df = dcast(Cars93, Manufacturer ~ Type, fun.aggregate = mean, value.var = "Price", fill = 0)
df


#문제4. for문을 사용해서 1~10까지의 누적합 구하기.
x = 0
for(i in 1:10){
  x = x + i
}
x


#문제5. Cars93 데이터와 Type 값을 넘겨받아 넘겨받은 Type의 평균 MPG.city값 돌려받는 함수 만들기
myFunction = function(data, typeName){
  temp = subset(data, data$Type == typeName)
  meanMpgCity = mean(temp$MPG.city)
  
  return(meanMpgCity)
}

aa = myFunction(Cars93, "Large")
aa


#문제6. 자동차 type별 Origin 비율구한 후 비율 누적 Bar Chart 그리기
df2 = Cars93 %>% group_by(Type, Origin) %>% summarise(n=n()) %>% mutate(ratio = n/sum(n))
ggplot(df2, aes(x=Type, y=ratio, fill=Origin)) + geom_bar(stat = "identity")


#문제7. 상위 20개 단어만 바차트로 보여주기
ggplot(head(arrange(wordDf,-freq),20), aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family="AppleGothic"))
