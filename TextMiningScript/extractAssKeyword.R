rm(list=ls())
gc()

start.time <- Sys.time()
source("/home/ruser/TextPrism/RSource/ML_functions.R")
source("/home/ruser/TextPrism/RSource/config_modeling.R")

#package check & install & load
libraryList <- c("dplyr","stringi","tm","caret","reshape","RODBC","RODBCext","doMC","pROC")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
require(dplyr)
require(stringi)
require(tm)
require(caret)
require(reshape)
require(RODBC)
require(RODBCext)
require(doMC)
require(pROC)
require(slam)
require(network)
require(sna)
require(ggplot2)
require(GGally)

registerDoMC(cores = coreNum)
sparse <- '0.98'

### Font Setting ###
#library(extrafont)
#font_import()
#fonts()
#loadfonts()

############################################
## Read in TM results
############################################
conn <- odbcConnect('ccaMart',uid='trendtracker',pwd='#tt1234')
#conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')

inputQuery <- "select crawl_data_id, crawled_date, keyword, ranking, role from t_tp_result_rank_namyun where user = 'baduc';"

tm<-sqlQuery(conn,inputQuery)

tm$keyword <- gsub(" ", "#", tm$keyword)
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "알파"), "알파고")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "알파고가"), "알파고")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "알파고와"), "알파고")
tm$keyword <- replace(tm$keyword, (tm$keyword %in% "이세돌과"), "이세돌")


disuse.term <- c("아다", "와의", "수로","서울","오후","어제","판후","디어","쓰다","금지","배포")
disuse.role <- c("OL")
tm <- tm[!(tm$role %in% disuse.role),]
tm <- tm[!(tm$keyword %in% disuse.term),]

odbcClose(conn)

## TM results into document keywords matrices
print("Make DTM")
tmKeyword <- fn_tm_keys(tm)
print(paste("Total Document :",nrow(tmKeyword)))

##Duplication Check
dupCheck <- duplicated(tmKeyword[,2])
tmKeyword <- tmKeyword[!dupCheck,]

## Make DTM
print(paste("Target Document :",nrow(tmKeyword)))
corp<-Corpus(DataframeSource(tmKeyword))
dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf), weighting = weightTfIdf))
dtm <- removeSparseTerms(dtm, as.numeric(sparse))

## Create asscoiation keyword ##
t1 <-findAssocs(dtm, c("이세돌"), 0.05)
terms <- as.data.frame(attributes(t1$이세돌))
weight <- as.data.frame(t1$이세돌)
weight <- weight$`t1$이세돌`
result <- cbind(terms, weight)

t2 <-findAssocs(dtm, c("알파고"), 0.05)
terms2 <- as.data.frame(attributes(t2$알파고))
weight2 <- as.data.frame(t2$알파고)
weight2 <- weight2$`t2$알파고`
result2 <- cbind(terms2, weight2)

assDf <- merge(result, result2, by.x="names", by.y="names", all.x = TRUE, all.y = TRUE)
assDf <- replace(assDf, is.na(assDf), 0)
colnames(assDf) <- c("term","이세돌","알파고")

## Convert a Data Frame to a Numeric Matrix  ##
new_dtmMatrix <- as.matrix(assDf[,2:3])
rownames(new_dtmMatrix)<- as.character(assDf[,1])
new_dtmMatrix[new_dtmMatrix>=0.02] <- 1
new_dtmMatrix[new_dtmMatrix<0.02] <- 0

## Make Network
net <- network(new_dtmMatrix, directed=FALSE)
net %v% "mode" <- ifelse(betweenness(net)>10, "big", "small")
col = c("small" = "grey", "big" = "gold")
ggnet2(net,mode = "kamadakawai", label=TRUE, color = "mode", palette = col)






