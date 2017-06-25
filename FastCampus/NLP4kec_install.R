# on Mac

# JDK 1.8 이상 설치 우선 진행
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre")
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')

install.packages("rJava")
install.packages("~/GitHub/NLP4kec_1.0.0.tgz", repos = NULL, type = .Platform$pkgType)

library(NLP4kec)

################################################################################################################
# on Window
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_131")

install.packages("rJava")
install.packages("C:/Users/user/Desktop/class/NLP4kec_1.0.0.zip" , repos=NULL, type="win.binary")

library(NLP4kec)