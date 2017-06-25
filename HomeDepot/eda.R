require(VennDiagram)
require(ggplot2)
require(dplyr)
require(sets)
require(ggplot2)
rid.newpage()

train <- read.csv(file = "./train.csv")
test <- read.csv(file = "./test.csv")
att <- read.csv(file = "./attributes.csv")
pd <- read.csv(file = "./product_descriptions.csv")
pd_new1 <- read.csv(file = "./pd1.csv")
pd_new2 <- read.csv(file = "./pd2.csv")
pd_new3 <- read.csv(file = "./pd3.csv")
pd_new4 <- read.csv(file = "./pd4.csv")
pd_new <- rbind(pd_new1, pd_new2)
pd_new <- rbind(pd_new, pd_new3)
pd_new <- rbind(pd_new, pd_new4)
rm(pd_new2)
gc()

suppressMessages(train_pd<- full_join(train, pd_new))

serchTermFreq <- train %>% group_by(search_term) %>% summarise(freq=n()) %>% arrange(desc(freq))
relevance_dist <- train %>% count(rel = as.factor(relevance))  %>% arrange(desc(n))

venn.plot1<-draw.pairwise.venn(
  length(train$product_uid),length(pd$product_uid),length(intersect(train$product_uid,pd$product_uid)),
  category = c("Train product_uid", "pd product_uid"),
  lty = rep("blank",2),
  fill = c("light blue", "pink"),
  alpha = rep(0.5, 2),
  cat.pos = c(0,0),
  cat.dist = rep(0.05, 2))

grid.newpage()
venn.plot2<-draw.triple.venn(area1 = length(test$product_uid), area2 = length(train$product_uid), area3 = length(att$product_uid), 
                             n12 = length(intersect(test$product_uid, train$product_uid)), 
                             n23 = length(intersect(train$product_uid, att$product_uid)),
                             n13 = length(intersect(test$product_uid, att$product_uid)),
                             n123 =length(intersect(att$product_uid, intersect(test$product_uid,train$product_uid))),
                             category = c("test", "train", "attribute"),
                             lty = "blank",
                             fill = c("skyblue", "pink1", "mediumorchid"))
grid.draw(venn.plot2)

grid.draw(venn.plot1)


test <- data.frame(tt=factor(train[1:5,"search_term"]))
test <- levels(droplevels(test$tt))
A <- gset(unlist(strsplit(test[1]," ")))

test2 <- as.matrix(pd_new)
#test2 <- data.frame(parsing=factor(pd_new[,"parsing"]))
#test2 <- levels(droplevels(test2$parsing))
B <- gset(unlist(strsplit(test2[1,2]," ")))
gset_similarity(A, B, "Jaccard")


for(i in 1:nrow(train)){
  pd = subset(pd_new, product_uid == as.character(train[i,2]), select = c(parsing))
  pd = tolower(pd[1,])
  pd_mat = as.matrix(pd)
  pd_set = gset(unlist(strsplit(pd_mat[1]," ")))
  
  title = train[i,3]
  title = tolower(title)
  title_mat = as.matrix(title)
  title_set = gset(unlist(strsplit(title_mat[1]," ")))
  
  st = train[i,4]
  st = tolower(st)
  st_mat = as.matrix(st)
  st_set = gset(unlist(strsplit(st_mat[1]," ")))
  
  js = gset_similarity(st_set, pd_set, "Jaccard")
  train$similty[i] <- js
  
  js2 = gset_similarity(st_set, title_set, "Jaccard")
  train$t_similty[i] <- js2
}


