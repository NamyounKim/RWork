library(dplyr)
library(lazyeval)
library(wordcloud2)
library(RColorBrewer)

source("../../ggRader.R")
source("../../ggRader2.R")

raw_review_weakness = merge(raw_review_weakness, raw_review %>% select(review_no, product_nm, site_product_cd, age_group, sex, site_cd), by.x="id", by.y="review_no", all.x = T)
raw_review_strength = merge(raw_review_strength, raw_review %>% select(review_no, product_nm, site_product_cd, age_group, sex, site_cd), by.x="id", by.y="review_no", all.x = T)


mean(raw_review_weakness$a1[which(raw_review_weakness$a1>0)])

makeSentimentalRadarChart = function(groupByCol, targetNm){
  #raw_review_weakness[,6:17] = apply(raw_review_weakness[,6:17], 2, function(x){replace(x,list=which(x==0),NA)})
  brand_score_weakness = raw_review_weakness %>% group_by_(groupByCol) %>% dplyr::summarise(발색 = mean(a1,na.rm = T),
                                                                                           커버력= mean(a2,na.rm = T),
                                                                                           밀착력_지속력= mean(a3,na.rm = T),
                                                                                           세정력= mean(a4,na.rm = T),
                                                                                           향= mean(a5,na.rm = T),
                                                                                           흡수력= mean(a6,na.rm = T),
                                                                                           보습력= mean(a7,na.rm = T),
                                                                                           안티에이징_미백= mean(a8,na.rm = T),
                                                                                           천연= mean(a9,na.rm = T),
                                                                                           휴대성= mean(a10,na.rm = T),
                                                                                           가성비= mean(a11,na.rm = T),
                                                                                           사용편리성= mean(a12,na.rm = T))
  brand_score_weakness[[groupByCol]] = paste0(brand_score_weakness[[groupByCol]], "_아쉬운점")
  
  #raw_review_strength[,6:17] = apply(raw_review_strength[,6:17], 2, function(x){replace(x,list=which(x==0),NA)})
  brand_score_strength = raw_review_strength %>% group_by_(groupByCol) %>% dplyr::summarise(발색 = mean(a1,na.rm = T),
                                                                                           커버력= mean(a2,na.rm = T),
                                                                                           밀착력_지속력= mean(a3,na.rm = T),
                                                                                           세정력= mean(a4,na.rm = T),
                                                                                           향= mean(a5,na.rm = T),
                                                                                           흡수력= mean(a6,na.rm = T),
                                                                                           보습력= mean(a7,na.rm = T),
                                                                                           안티에이징_미백= mean(a8,na.rm = T),
                                                                                           천연= mean(a9,na.rm = T),
                                                                                           휴대성= mean(a10,na.rm = T),
                                                                                           가성비= mean(a11,na.rm = T),
                                                                                           사용편리성= mean(a12,na.rm = T))
  brand_score_strength[[groupByCol]] = paste0(brand_score_strength[[groupByCol]], "_좋은점")
  
  brand_score_merge = rbind(brand_score_strength, brand_score_weakness)
  colnames(brand_score_merge)[1] = "groupNm"

  targetBrand = targetNm
  sub_brand_score_merge = brand_score_merge %>% filter(groupNm %in% c(paste0(targetBrand,"_좋은점"), paste0(targetBrand,"_아쉬운점")))
  ggRadar2(sub_brand_score_merge, aes(colour=groupNm), rescale = F, ylim = 0.1)
}

# Radar Chart 만들기
makeSentimentalRadarChart(groupByCol = "brand_nm", targetNm = "이니스프리")
makeSentimentalRadarChart(groupByCol = "age_group", targetNm = 40)
makeSentimentalRadarChart(groupByCol = "product_nm", targetNm = "더마 리페어 시카크림")

#로즈워터 토너
#더마 리페어 시카크림

# 각 속성별 브랜드, 상품 순위보기
targetBrands = raw_review_strength %>% group_by(brand_nm) %>% dplyr::summarise(n=n()) %>% filter(n>20) %>% select(brand_nm)
targetProduct = raw_review_strength %>% group_by(product_nm) %>% dplyr::summarise(n=n()) %>% filter(n>=20) %>% select(product_nm)
  
weakness_output = data.frame(rankNo = seq(1:18))
for(i in 1:12){
  col_index = paste0("a",i)
  temp = raw_review_weakness %>% filter(brand_nm %in% targetBrands$brand_nm) %>% group_by(brand_nm) %>% 
    dplyr::summarise_(tt= interp(~mean(v), v= as.name(col_index))) %>% arrange(-tt)
  colnames(temp) = c(paste0("nm",i), col_index)
  weakness_output = cbind(weakness_output, temp)
}

strength_output = data.frame(rankNo = seq(1:18))
for(i in 1:12){
  col_index = paste0("a",i)
  temp = raw_review_strength %>% filter(brand_nm %in% targetBrands$brand_nm) %>% group_by(brand_nm) %>% 
    dplyr::summarise_(tt= interp(~mean(v), v= as.name(col_index))) %>% arrange(-tt)
  colnames(temp) = c(paste0("nm",i), col_index)
  strength_output = cbind(strength_output, temp)
}

weakness_prd_output = data.frame(rankNo = seq(1:nrow(targetProduct)))
for(i in 1:12){
  col_index = paste0("a",i)
  temp = raw_review_weakness %>% filter(product_nm %in% targetProduct$product_nm) %>% group_by(product_nm) %>% 
    dplyr::summarise_(tt= interp(~mean(v), v= as.name(col_index))) %>% arrange(-tt)
  colnames(temp) = c(paste0("nm",i), col_index)
  weakness_prd_output = cbind(weakness_prd_output, temp)
}

strength_prd_output = data.frame(rankNo = seq(1:nrow(targetProduct)))
for(i in 1:12){
  col_index = paste0("a",i)
  temp = raw_review_strength %>% filter(product_nm %in% targetProduct$product_nm) %>% group_by(product_nm) %>% 
    dplyr::summarise_(tt= interp(~mean(v), v= as.name(col_index))) %>% arrange(-tt)
  colnames(temp) = c(paste0("nm",i), col_index)
  strength_prd_output = cbind(strength_prd_output, temp)
}


# 각 브랜드 속성별 주요 키워드
makeWordVecModel = function(inputData, filterName, vectorsSize= 100, windowSize = 4){
  inputData = inputData %>% filter(brand_nm == filterName)
  
  write.table(inputData$parsedContent, file = "./input_review.txt", row.names = F, col.names = F, quote = F)
  
  input_model = train_word2vec(train_file = "./input_review.txt"
                               , threads = 8
                               , vectors = vectorsSize
                               , window = windowSize)
  
  input_model = input_model[rownames(input_model) != "</s>",]
  input_model = input_model[nchar(rownames(input_model)) > 1,]
  
  return(input_model)
}
input_model = makeWordVecModel(inputData = raw_review_strength, filterName = "아리따움", vectorsSize = 200, windowSize=4)
wordDf = nearest_to(input_model, input_model[["발색"]], 50)
wordDf = (1-(as.data.frame(wordDf))) * 10
wordDf$word = row.names(wordDf)
wordDf = wordDf[,c(2,1)]


colfunc = colorRampPalette(c("royalblue", "white"))

wordcloud2(data = wordDf
           , color = colfunc(50)
           , shape = "circle"
           , minRotation = 0
           , gridSize =10
           , size = 0.3
           , fontFamily = "나눔고딕"
           #, fontWeight = 300
           )

#------------------  아쉬운점 -------------------------
input_model = makeWordVecModel(inputData = raw_review_weakness, filterName = "아리따움", vectorsSize = 200, windowSize=4)
wordDf = nearest_to(input_model, input_model[["밀착력"]], 50)
wordDf = (1-(as.data.frame(wordDf))) * 10
wordDf$word = row.names(wordDf)
wordDf = wordDf[,c(2,1)]

colfunc = colorRampPalette(c("firebrick3", "white"))

wordcloud2(data = wordDf
           , color = colfunc(50)
           , shape = "circle"
           , minRotation = 0
           , gridSize =10
           , size = 0.3
           , fontFamily = "나눔고딕"
           #, fontWeight = 300
)


#-----------------------------------
temp = raw_review_strength %>% group_by(product_nm) %>% dplyr::summarise(n=n()) %>% arrange(-n) %>% head(10)
ggplot(temp, aes(x=reorder(product_nm,n), y=n)) + geom_bar(stat = "identity") + coord_flip()

#-----------------------------------
temp = raw_review_strength %>% group_by(regDate) %>% dplyr::summarise(n=n())
ggplot(temp, aes(x=as.character(regDate), y=n, group=1))  + 
  geom_line(colour="grey20", size=1) + 
  geom_point(colour="grey20", size=3) +     # Set title
  theme_bw() +
  theme(legend.position=c(.7, .4))+ theme(axis.text.x=element_text(angle = 45, hjust = 1)) # x축 텍스트 회전시키기

raw_review_strength %>% filter(brand_nm %in% targetBrands$brand_nm) %>% group_by(brand_nm) %>% dplyr::summarise(커버력= mean(a2)) %>% arrange(-커버력)







ggplot(raw_review_strength, aes(x=brand_nm, y=a2)) + geom_boxplot()


brand_score_weakness = brand_score_weakness %>% filter(brand_nm %in% c("설화수","헤라","프리메라"))
brand_score_weakness = brand_score_weakness %>% filter(brand_nm %in% c("라네즈","아이오페","마몽드"))
brand_score_weakness = brand_score_weakness %>% filter(brand_nm %in% c("에뛰드하우스", "이니스프리 본품","아리따움"))

ggRadar2(brand_score_weakness, aes(colour=brand_nm), rescale = F, ylim = 0.1)


brand_score_strength = brand_score_strength %>% filter(brand_nm %in% c("설화수","헤라","프리메라"))
brand_score_strength = brand_score_strength %>% filter(brand_nm %in% c("라네즈","아이오페","마몽드"))
brand_score_strength = brand_score_strength %>% filter(brand_nm %in% c("에뛰드하우스", "이니스프리 본품","아리따움"))

ggRadar2(brand_score_strength, aes(colour=brand_nm), rescale = F, ylim = 0.1)
