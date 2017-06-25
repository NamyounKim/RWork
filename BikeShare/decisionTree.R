library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

seed <- 100
f=rpart(count~hour,data=train, control=rpart.control(minbucket = 900 ,cp=0.001))
fancyRpartPlot(f)
plotcp(f)

registered=rpart(registered~hour,data=train, control=rpart.control(minbucket = 900 ,cp=0.0075))
fancyRpartPlot(registered)
plotcp(registered)

mean(train[train$humidity < 13, "registered"])











