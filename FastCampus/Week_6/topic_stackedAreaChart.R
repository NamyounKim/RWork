library(ggplot2)

# 원본 데이터에 토픽 번호 붙이기
textData$topicNo = id_topic$doc_topic

# 일자별 토픽번호별 문서 건수 구하기
dateTopic = textData %>% group_by(startDate, topicNo) %>% summarise(n = n()) %>% arrange(topicNo, startDate)
# 식별ID 만들기
dateTopic$id = paste0(dateTopic$startDate,"_",dateTopic$topicNo)

# 토픽번호별 문서가 없는 일자를 핸들링하기 위한 작업
allDate = seq(min(dateTopic$startDate), max(dateTopic$startDate), by="day")
allDate = data.frame(startDate = allDate)
topicNo = data.frame(topicNo = seq(1,max(textData$topicNo),1))
allDate = merge(allDate, topicNo, all=T)
allDate$id = paste0(allDate$startDate,"_",allDate$topicNo)

# 토픽번호별 문서가 없는 일자에 0으로 채우기
mergeData = merge(allDate, dateTopic, by = "id", all.x = T)
mergeData[is.na(mergeData$n),"n"] = 0

# 차트 그리기
ggplot(mergeData, aes(x=startDate.x, y=n, fill=as.factor(topicNo.x))) + geom_area() + geom_line(position = "stack")

# 차트 위에 라벨 붙이기
ggplot(mergeData, aes(x=startDate.x, y=n, fill=as.factor(topicNo.x))) + geom_area() + geom_line(position = "stack") + 
  geom_text(aes(label = as.factor(topicNo.x)), position = position_stack(vjust = 0.5))


