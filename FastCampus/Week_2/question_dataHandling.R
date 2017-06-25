# 문제1. 엔진사이즈(EngineSize)가 3.0 이상이고, 유형(Type)이 "Large"인 행만 추출하기

# 문제2. 위에서 만든 데이터에서 제조사(Manufacture), 모델(Model), 가격(Price) 열만 추출하기

# 문제3. 2번에서 만든 데이터의 컬럼명을 "A", "B", "C" 로 변경하기

# 문제4. 전체 데이터에서 자동차 제조사(Manufacture) 별로 평균 가격 구하기

# 문제5. 위의 문제를 dplyr 패키지로 구해보기

# 문제6. for문을 사용해서 1~20 까지의 짝수의 합 구하기.
x = 0
for(i in 1:20){
  if((i %% 2) == 0){
    x = x + i
  }
}
x

# 문제7. Cars93 데이터와 Type 값을 넘겨받아 넘겨받은 Type의 평균 MPG.city값 돌려받는 함수 만들기
myFunction = function(data, typeName){
  temp = subset(data, data$Type == typeName)
  meanMpgCity = mean(temp$MPG.city)
  
  return(meanMpgCity)
}

aa = myFunction(Cars93, "Large")
aa

# 문제8. 자동차 type별 Origin 비율구한 후 비율 누적 Bar Chart 그리기
df2 = Cars93 %>% group_by(Type, Origin) %>% summarise(n=n()) %>% mutate(ratio = n/sum(n))
ggplot(df2, aes(x=Type, y=ratio, fill=Origin)) + geom_bar(stat = "identity")

# 문제9. 상위 20개 단어만 바차트로 보여주기
ggplot(head(arrange(wordDf,-freq),20), aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(family="AppleGothic"))
