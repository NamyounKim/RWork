# 1. 기본 유틸리티 ------------------------------------------------------------------------------------------------------
#패키지 설치
install.packages("readr")

#여러개 패키지 한번에 설치
install.packages(c("readr","MASS", "readxl"))

# 설치가능한 패키지 리스트 보기
# https://cran.r-project.org/web/packages/index.html

#패키지 불러오기
library(readr)
library(readxl)

#작업 디렉토리 위치 확인
#작업 디렉토리는 절대 클라우드 드라이브에 위치하면 안됨
#또한 경로 내에 한글로 된 경로가 있으면 안됨
getwd()

#메모리 비우기
gc()

#R버전 확인
R.version

#R환경 변수 확인
Sys.getenv()

#변수만들기 및 삭제
x = 1
rm(x)

#패키지의 있는 함수명 확인하기
readr:: #패키지명 뒤에 ::를 붙이면 패키지에 포함된 내용을 보여준다.
  
# 한글 깨질경우 터미널에서 아래 라인 실행 (MAC 경우)
# defaults write org.R-project.R force.LANG en_US.UTF-8
  
# 2. Java 설정 관련 ---------------------------------------------------------------------------------------------------------
# rJava 패키지 설치
install.packages("rJava")

# rJava 불러오기
library(rJava)

#JAVA버전 확인
.jinit()
.jcall("java/lang/System","S","getProperty","java.runtime.version")

#JAVA_HOME 위치 보기
Sys.getenv("JAVA_HOME")

#JAVA_HOME 설정 (윈도우 경우)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_121/")

#JAVA_HOME 설정 (MAC 경우)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre/lib/server/libjvm.dylib')

  
# 3. 파일 import, export ------------------------------------------------------------------------------------------------

#CSV파일 읽어오기
read_csv(file = "CSV 파일위치", col_names = T) #각 파라미터는 콤마로 구분된다.

#엑셀 파일 읽어오기
read_excel(path = "엑셀 파일 위치", sheet = "시트명 또는 번호", col_names = T)

#CSV파일 내보내기
write_csv(x = 데이터Set, path = "저장하고자 하는 위치와 파일명")

#현재 데이터 정보 캡쳐하기(모두 저장하기)
save.image("./totalData.RDATA")

#RDATA 파일 읽어오기
load("./totalData.RDATA")

#특정 변수(데이터Set) 하나만 RDS 형식으로 저장하기
saveRDS(데이터Set, "저장하고자 하는 위치와 파일명")

#저장한 변수(데이터Set) 읽어오기
svmModel = readRDS("읽어오고자 하는 파일명")



