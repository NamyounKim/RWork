#변수 저장 방법
saveRDS(svmModel, "./svmModel.rds")

#저장한 변수 읽어오기
svmModel2 = readRDS("./svmModel.rds")

#모든 변수 한번에 저장하기
save.image("./totalData.RDATA")

#RDATA 파일 읽어오기
load("./totalData.RDATA")
