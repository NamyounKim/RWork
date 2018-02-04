# 패키지 설치
install.packages("dplyr")

# 패키지 로드
library(dplyr) 

# 메모리 정리
gc()

# 변수에 값 저장
a = 1
a <- 1

# 기본 연산
a + 1
a + a
a - 2

#변수 저장 방법
saveRDS(a, "./a.RDS")

#저장한 변수 읽어오기
a2 = readRDS("./a.RDS")

#모든 변수 한번에 저장하기
save.image("./totalData.RDATA")

#RDATA 파일 읽어오기
load("./totalData.RDATA")

