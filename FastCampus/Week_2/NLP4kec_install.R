
## 윈도우인 경우 ------------------------------------------------------------------------------------------------------------
install.packages("rJava")
install.packages("본인PC의 경로/NLP4kec_1.2.0.zip" , repos=NULL, type="win.binary") # 본인 PC경로로 세팅해준다.

library(NLP4kec)

# 안될 경우 아래 변수 세팅 실행 후 다시 설치
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_181") # 본인 PC경로로 세팅해준다.





## 맥북인 경우 ---------------------------------------------------------------------------------------------------------------
install.packages("rJava")
install.packages("본인PC의 경로/NLP4kec_1.2.0.tgz", repos = NULL, type = .Platform$pkgType) # 본인 PC경로로 세팅해준다.

library(NLP4kec)

# 안될 경우 아래 변수 세팅 실행 후 다시 설치
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre") # 본인 PC경로로 세팅해준다.
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre/lib/server/libjvm.dylib') # 본인 PC경로로 세팅해준다.






