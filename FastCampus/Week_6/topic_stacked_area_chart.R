library(ggplot2)
library(dplyr)

textData = readRDS("./raw_data/petitions_content_2018.RDS")

# 원본 데이터에 토픽 번호 붙이기
textData$topicNo = id_topic$doc_topic

# 일자별 토픽번호별 문서 건수 구하기
dateTopic = textData %>% group_by(start_date, topicNo) %>% summarise(n = n()) %>% arrange(topicNo, start_date)

# 식별ID 만들기
dateTopic$id = paste0(dateTopic$start_date,"_",dateTopic$topicNo)

# 토픽번호별 문서가 없는 일자를 핸들링하기 위한 작업
allDate = seq(from = as.Date(min(dateTopic$start_date))
              ,to =  as.Date(max(dateTopic$start_date))
              , by="day")
allDate = data.frame(start_date = allDate)
topicNo = data.frame(topicNo = seq(1, max(textData$topicNo, na.rm = T), 1))
allDate = merge(allDate, topicNo, all=T)
allDate$id = paste0(allDate$start_date,"_", allDate$topicNo)

# 토픽번호별 문서가 없는 일자에 0으로 채우기
mergeData = merge(allDate, dateTopic, by = "id", all.x = T)
mergeData[is.na(mergeData$n),"n"] = 0

# 차트 그리기
ggplot(mergeData, aes(x=as.character(start_date.x), y=n, fill=as.factor(topicNo.x), group = as.factor(topicNo.x) )) + 
  geom_area() + geom_line(position = "stack") + theme(axis.text.x=element_text(angle = 45, hjust = 1))

# 특정 토픽만 선택해서 그리기
ggplot(mergeData %>% filter(topicNo.x %in% c(1,2,3,4,5)), aes(x=as.character(start_date.x), y=n, fill=as.factor(topicNo.x), group = as.factor(topicNo.x) )) + 
  geom_area() + geom_line(position = "stack") + theme(axis.text.x=element_text(angle = 45, hjust = 1))

#ggplot(dateTopic, aes(x=startDate, y=n, fill=as.factor(topicNo))) + geom_area() + geom_line(position = "stack")

# 차트 위에 라벨 붙이기
ggplot(mergeData, aes(x=start_date.x, y=n, fill=as.factor(topicNo.x))) + geom_area() + geom_line(position = "stack") + 
  geom_text(aes(label = as.factor(topicNo.x)), position = position_stack(vjust = 0.5))


