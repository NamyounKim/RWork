
# 원문 데이터 가져오기
textData = readRDS(file = "./raw_data/petitions_content_2018.RDS")
categoryData = readRDS(file = "./raw_data/petitions_category_name_2018.RDS")

#문제1) 각 데이터에 어떤 컬럼이 있는지 확인
colnames(textData)
colnames(categoryData)

#문제2) 각 데이터의 컬럼 정보 확인
str(textData)
str(categoryData)

#문제3) 각 데이터의 row개수 구하기
nrow(textData)
nrow(categoryData)

#문제4) 원문과 문서 카테고리 데이터 Join하기
textData_join = merge(x = textData, y = categoryData, by = "doc_id")

#문제5) 일별 문서개수 구하고, Line차트로 일별 추이 구하기
temp = textData_join %>% group_by(start_date) %>% summarise(doc_n=n())
ggplot(temp, aes(x=start_date, y=doc_n, group = 1)) + geom_line()

#문제6) 카테고리별 문서개수 구하고, Bar차트로 그리기
textData_join %>% group_by(category_name) %>% summarise(doc_n=n())

#문제7) 동의수가 높은순으로 10개 문서의 문서id, 제목, 동의수 컬럼만 가져오기
textData_join %>% arrange(-agree_count) %>% select(doc_id, title, agree_count) %>% head(10)

#문제8) 카테고리별 문서개수와 평균동의수 구한 후 평균동의수로 내림차순 정렬하기
textData_join %>% group_by(category_name) %>% summarise(doc_n = n(), agree_avg = mean(agree_count)) %>% arrange(-agree_avg)

#문제9) 카테고리별 문서당 동의개수를 구하기
textData_join %>% group_by(category_name) %>% summarise(doc_n = n(), agree_sum = sum(agree_count)) %>% mutate(agree_per_doc = agree_sum/doc_n) %>% arrange(-agree_per_doc)

#문제10) 2018년 4월에 나온 청원글의 카테고리별 문서개수 구하고 내림차순으로 정렬하기
textData_join %>% filter(start_date >= "2018-04-01" & start_date <= "2018-04-30") %>% group_by(category_name) %>% summarise(doc_n=n()) %>% arrange(-doc_n)


