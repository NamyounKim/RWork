#문제6. 자동차 type별 Origin 비율구한 후 비율 누적 Bar Chart 그리기
df2 = Cars93 %>% group_by(Type, Origin) %>% summarise(n=n()) %>% mutate(ratio = n/sum(n))
ggplot(df2, aes(x=Type, y=ratio, fill=Origin)) + geom_bar(stat = "identity")