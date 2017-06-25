library(ggplot2)
library(psych)

#Box plot
ggplot(train, aes(x=factor(hour), y=registered, fill=factor(year))) + geom_boxplot(outlier.colour = "RED") + stat_summary(fun.y=mean,geom = "point", size=2) + theme(axis.text=element_text(size=12), legend.position="bottom") 
ggplot(train[train$year=='2012',], aes(x=factor(hour), y=count, fill=factor(workingday))) + geom_boxplot() + theme(axis.text=element_text(size=12))
ggplot(train, aes(x=factor(weather), y=count)) + geom_boxplot() + theme(axis.text=element_text(size=12))


#Scatter Plot
#p<-ggplot(train, aes(x=season, y=weather, size=rentCount, colour=temp))
#p + geom_point() + scale_size_area(max_size = 25) + scale_colour_gradient(low="lightblue", high="red")+ theme(axis.text=element_text(size=12))

pairs.panels(subset(train, select = c(count,diffTemp)))
pairs.panels(subset(train, diffTemp <=0 ,select = c(count,casual,registered,temp,atemp,diffTemp)))

#Bar Plot
ggplot(temp, aes(x=factor(hour), y=rentCount, fill=category))+ geom_bar(stat = "identity", width = 0.8, position = "dodge") + theme(axis.text=element_text(size=12)) + scale_fill_brewer(palette="Set2")


#Data row check
train_m <- train %>% group_by(hour) %>% summarise(rentCount = mean(count)) %>% mutate(category = "real")
result_m <- resultTemp %>% group_by(hour) %>% summarise(rentCount = mean(count)) %>% mutate(category ="pred")
temp <- rbind(as.data.frame(train_m), as.data.frame(result_m))
train_m



test <- aov(count ~ season, data=train)
summary(test)
testDf<- fitted(test)
testDf <- as.data.frame(testDf)
testDf$resid <- resid(test)
ggplot(testDf, aes(x=testDf, y=resid)) + geom_point()
ggplot(testDf, aes(sample = resid)) + stat_qq()
