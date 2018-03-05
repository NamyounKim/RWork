
  par(mfrow=c(2,2))
  barplot(colMeans(rawData %>% filter(brand=="설화수") %>% select(9:18)), ylim = c(0,0.5))
  barplot(colMeans(rawData %>% filter(brand=="아이오페") %>% select(9:18)), ylim = c(0,0.5))
  barplot(colMeans(rawData %>% filter(brand=="헤라") %>% select(9:18)), ylim = c(0,0.5))
  barplot(colMeans(rawData %>% filter(brand=="프리메라") %>% select(9:18)), ylim = c(0,0.5))
  
  par(mfrow=c(2,2))
  barplot(colMeans(rawData %>% filter(brand=="이니스프리") %>% select(9:18)), ylim = c(0,0.5))
  barplot(colMeans(rawData %>% filter(brand=="에뛰드 하우스") %>% select(9:18)), ylim = c(0,0.5))
  barplot(colMeans(rawData %>% filter(brand=="아리따움") %>% select(9:18)), ylim = c(0,0.5))
  barplot(colMeans(rawData %>% filter(brand=="마몽드") %>% select(9:18)), ylim = c(0,0.5))

# 브랜드별 리뷰수
temp = rawData %>% group_by(brand) %>% summarise(n=n()) %>% arrange(-n)
temp$brand[temp$n>2]

# 가성비 순위
rank1 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(발색)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank2 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(커버)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank3 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(밀착력)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank4 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(세정력t)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank5 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(흡수력)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank6 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(보습)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank7 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(주름)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank8 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(영양)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank9 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(휴대)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])
rank10 = rawData %>% group_by(brand) %>% summarise(score_mean = mean(가성비t)) %>% arrange(-score_mean) %>% filter(brand %in% temp$brand[temp$n>2])

rank = cbind(rank1,rank2,rank3,rank4,rank5,rank6,rank7,rank8,rank9,rank10)
write.csv(rank, "./rank.csv", row.names = F)


# product 별 평정 산출
rawData %>% group_by(product) %>% dplyr::summarise(n=n()) %>% arrange(-n)
# 평점 구간 정하기
attName = colnames(docScore)[1:10]
par(mfrow=c(2,5))
for(i in 1:10){
  aname = attName[i]
  temp = rawData %>% filter(product =="페이셜 마일드 필링") %>% select(aname)
  barplot(quantile(temp[,1], seq(0,1,0.2)), xlim = c(0,1), horiz = T, main = aname)
}



p_score = rawData %>% filter(product =="노세범 미네랄 파우더") %>% summarise(발색 = mean(발색)
                                                                            , 커버= mean(커버)
                                                                            , 밀착력= mean(밀착력)
                                                                            , cleansing= mean(cleansing)
                                                                            , 흡수력= mean(흡수력)
                                                                            , 보습= mean(보습)
                                                                            , 주름= mean(주름)
                                                                            , 영양= mean(영양)
                                                                            , 휴대= mean(휴대)
                                                                            , vfm= mean(vfm))
p_score
ggRadar(data = p_score)
