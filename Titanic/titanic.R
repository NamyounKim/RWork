require(dplyr)
require(stringi)
require(ggplot2)
require(psych)

train <- read.csv("./train.csv")
test <- read.csv("./test.csv")

head(train)

train_r <- train %>% group_by(SibSp,Survived) %>% summarise(count=n()) %>% mutate(ratio=count/sum(count))
train_Parch <- train %>% group_by(Parch,Survived) %>% summarise(count=n()) %>% mutate(ratio=count/sum(count))
train_Pclass <- train %>% group_by(Pclass,SibSp,Survived) %>% summarise(count=n()) %>% mutate(ratio=count/sum(count))
train_r
train_r[train_r$Survived==1,]
train_p[train_p$Survived==1,]

ggplot(train[train$Parch!='0',], aes(x=factor(Pclass), fill=factor(Parch)))+geom_bar()

ggplot(train, aes(x=factor(), y=Age))+ geom_boxplot()

ggplot(train, aes(Age, fill=factor(Survived)))+ geom_histogram(alpha=.5, position="identity")

ggplot(train_Parch, aes(x=factor(Parch), y=ratio, fill=factor(Survived)))+geom_bar(stat = "identity")
ggplot(train_Pclass, aes(x=factor(SibSp), y=count, fill=factor(Pclass)))+geom_bar(stat = "identity")

#버블차트
p<-ggplot(train_r, aes(x=SibSp, y=factor(Survived), size=count, colour=ratio))
p + geom_point() + scale_size_area(max_size = 25) + scale_colour_gradient(low="white", high="red")+ theme(axis.text=element_text(size=12))

summary(train$Survived)


#여자와 어린아이가 많이 살았다
#1st Class 사람들이 많이 살았다
#부모/자식이 없이 탑승한 사람들이 상당히 많다. 하지만 생존 비율로 보면 1~3명 정도의 부모/자식이 있는 사람이 많이 생존함
#형제자매/남편 또는 부인 없이 탑승한 사람들이 상당히 많다. 하지만 `1~2명 정도의 형제자매/남편 or 부인이 있는 사람이 많이 생존함


library(ca)
tt <- table(train$Sex, train$Survived)
mytable <- with(train, table(train$Pclass, train$Parch)) # create a 2 way table
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages

fit <- ca(mytable)
print(fit) # basic results 
summary(fit) # extended results 
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map ="rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
summary(train)

ggplot(train[train$Parch!='0',], aes(x=factor(Pclass), fill=factor(Parch)))+geom_bar()
#등급이 낮은 사람들의 자녀수가 많다.






###################################
seed <- 1000
train2 <- subset(train , select= -c(Ticket,Name,Cabin))
test2 <- subset(test, select = -c(Ticket,Name,Cabin))
train2$Survived <- factor(train2$Survived)
f=rpart(Survived~.,data=train2, control=rpart.control(cp=0.01))
fancyRpartPlot(f)
plotcp(f)


pred <- predict(f, test2)


registered=rpart(registered~hour,data=train, control=rpart.control(minbucket = 900 ,cp=0.0075))
fancyRpartPlot(registered)
plotcp(registered)

mean(train[train$humidity < 13, "registered"])








