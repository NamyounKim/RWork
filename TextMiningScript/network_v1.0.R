rm(list=ls())
gc()

#package check & install & load
libraryList <- c("dplyr","stringi","tm","reshape","slam","igraph","network","sna")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
require(dplyr)
require(stringi)
require(tm)s
require(reshape)
require(slam)
require(igraph)
require(network)


load(file="/home/ruser/TextPrism/output/")

##Build a Graph
makeNetwork <- function(networkMatrix){
  g <- graph.adjacency(networkMatrix, weighted=T, mode = "undirected")
  g <- simplify(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  
  set.seed(3952)
  layout1 <- layout.kamada.kawai(g)
  layout2 <- layout.graphopt(g)
  plot.igraph(g, layout=layout1,edge.width=2, main="TopicKeword Network", vertex.size=10*sqrt(hub.score(g)$vector))
  
  write.graph(g,file ="test.ncol",format="ncol")
}
############################################
## Read in TM results
############################################


## TM results into document keywords matrices
#corp<-Corpus(DataframeSource(tmKeyword))
#tdm<-TermDocumentMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
#tdm<-removeSparseTerms(tdm,0.8)
#termDocMatrix <- as.matrix(tdm)
#termDocMatrix[termDocMatrix>=1] <- 1
#termMatrix <- termDocMatrix %*% t(termDocMatrix)

##topicTDM
topicTDM <- posterior(lda_tm)$term

smallTopicTDM <- topicTDM[,colSums(topicTDM)>0.2]

smallTopicMatrix <- as.matrix(smallTopicTDM)
smallTopicMatrix[smallTopicMatrix>=0.01] <- 1
smallTopicMatrix[smallTopicMatrix<0.01] <- 0
smallTopicMatrix <-  t(smallTopicMatrix) %*% smallTopicMatrix

smallTopicMatrix2 <-  smallTopicMatrix %*% t(smallTopicMatrix)

makeNetwork(smallTopicMatrix2)




