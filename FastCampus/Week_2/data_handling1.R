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
dim(mat) #매트릭스 dimension 값 구하기
mat[1,2] #1행 2열 값 가져오기

############################################
# 패키지 설치 및 불러오기
############################################
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
iris[,1] # 첫번째 열(column) 값 가져오기
iris[,2] # 두번째 열(column) 값 가져오기
iris[,1:3] # 첫번째부터 세번째 열 가져오기

iris$Sepal.Length # Sepal.Length라는 이름을 갖는 열(column) 값 가져오기
subset(iris, select = c(Sepal.Length)) # Subset 함수로 Sepal.Length 라는 이름을 갖는 열(column) 값 가져오기
subset(iris, select = c(Sepal.Length, Sepal.Width)) # Subset 함수로 Sepal.Length, Sepal.Width 라는 이름을 갖는 열(column) 값 가져오기

#3-2.특정 행값만 가져오기
iris[1,] # 첫번째 행(row) 값 가져오기
iris[2,] # 두번째 행(row) 값 가져오기
iris[1:3,] # 첫번째부터 세번째 행 가져오기
iris[4,2] # 네번째 행의 2번째 열 값 가져오기 
subset(iris, iris$Sepal.Width > 4) # Sepal.Width 가 4보다 큰 행만 가져오기 

#3-3. 특정 행, 열값만 가져오기
iris[3,4] # 3번째 행의 4번째 열 값 가져오기
subset(iris, iris$Sepal.Width > 4,  select = c(Sepal.Length)) # Sepal.Width 가 4보다 큰 행에서 Sepal.Length 열만 가져오기

#3-4. 서로 같은 값(하나만) 매칭하여 가져오기
subset(iris, iris$Species == "setosa")

#3-5. 서로 같은 값들 매칭하여 가져오기
subset(iris, iris$Species %in% c("setosa","virginica"))


#4-1.행, 열개수 확인하기
nrow(iris)
ncol(iris)

#4-2. Vector, List 의 길이 구하기
length(iris[,1])
length(iris[1,])

#5.행 추가하기
addRow = list(10, 20, 10, 20, "virginica")
iris = rbind(iris, addRow)

#6.열 추가하기
addCol = sample(1:151, 151)
iris = cbind(iris, addCol)

#7.열 이름 불러오기
colnames(iris)

#8.열 이름 바꾸기
colnames(iris)[6] = "ChangeColName" # 6번째 열이름 바꾸기
colnames(iris)

colnames(iris) = c("a","b","c","d","e","f") # 모든 열이름 바꾸기
colnames(iris)

#9.Group by
tapply(iris$Sepal.Length, iris$Species, mean) # Species별로 Sepal.Length의 평균 구하기

#10.Data Join
df1 = data.frame(id = c(1,2,3,4,5,6), 
                 name=c("Jonh", "Jessica", "Tom","Rodrio","James","Alessia"))
df2 = data.frame(id=c(2,4,6,8), 
                 location=c("Seoul","LA", "Paris","Rome"))

#11-1.Inner Join
merge(df1, df2, by="id")

#11-2. Left Join
merge(df1, df2, by="id", all.x = TRUE)

#11-3 Right Join
merge(df1, df2, by="id", all.y = TRUE) 

#11-4 조인키가 서로 다를때
merge(df1, df2, all.x = TRUE, by.x = "dd", by.y = "aa")

#12.Data Export
write.csv(iris, file = "./iris.csv", row.names = FALSE) 
write.table(iris, file = "./iris.txt", row.names = FALSE, quote = F, sep = ",")

#13. Data Read
install.packages("readr")
library(readr)
read.csv()
temp = read_csv(file = "./dictionary/stopword_ko.csv")

install.packages("readxl")
library(readxl)
read_xlsx()

getwd()


