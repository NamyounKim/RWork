#Vector 만들기
a = c(1,2,4)

#List 만들기
b = list("test", 5, a)

#Matrix 만들기
mat = matrix( c(2, 4, 3, 1, 5, 7), # 데이터
            nrow=2,         # 행의수
            ncol=3,         # 열의수 
            byrow = TRUE)   # 행기준으로 만들기

#matrix handling
dim(mat)
mat[1,2]

############################################
# 패키지 설치 및 불러오기
############################################기
install.packages("MASS")
library(MASS)

############################################
# 기본 데이터 핸들링
############################################
#1.데이터 프레임형식의 샘플 데이터 가져오기
data("iris")
iris

data("Cars93")
Cars93

#2.데이터 형식 확인하기
str(iris)
str(Cars93)

#3.데이터에 접근하기

#3-1.특정 열값만 가져오기
iris[,1]
iris$Sepal.Length
subset(iris, select = c(Sepal.Length))
subset(iris, select = c(Sepal.Length, Sepal.Width))

#3-2.특정 행값만 가져오기
iris[1,]
subset(iris, row.names(iris) == "1")
subset(iris, iris$Sepal.Width > 4)

#4.행, 열개수 확인하기
nrow(iris)
ncol(iris)

#5.행 추가하기
addRow = list(10, 20, 10, 20, "virginica")
iris = rbind(iris, addRow)

#6.열 추가하기
addCol = sample(1:151, 151)
iris = cbind(iris, addCol)

#7.열 이름 불러오기
colnames(iris)

#8.열 이름 바꾸기
colnames(iris)[6] = "ChangeColName"
colnames(iris) = c("a","b","c","d","e","f")
colnames(iris)

#9.Group by
tapply(iris$Sepal.Length, iris$Species, mean)

#10.Data Join
df1 = data.frame(id = c(1,2,3,4,5,6), 
                 name=c("Jonh", "Jessica", "Tom","Rodrio","James","Alessia"))
df2 = data.frame(id=c(2,4,6,8), 
                 location=c("Seoul","LA", "Paris","Rome"))

#10-1.Inner Join
merge(df1, df2, by="id")

#10-2. Left Join
merge(df1, df2, all.x = TRUE)

#조인키가 서로 다를때
merge(df1, df2, all.x = TRUE, by.x = "dd", by.y = "aa")

#10-3 Right Join
merge(df1, df2, all.y = TRUE) 


#11.Data Export
write.csv(Cars93, file = "./Car93_Export.csv", row.names = FALSE)




