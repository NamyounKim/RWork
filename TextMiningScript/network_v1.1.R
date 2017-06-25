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
require(tm)
require(ggplot2)
require(GGally)
require(reshape)
require(slam)
require(igraph)
require(network)
require(sna)

makeNetwork <- function(lda_tm){
  topicTDM <- posterior(lda_tm)$term
  
  smallTopicTDM <- topicTDM[,colSums(topicTDM)>0.01]
  smallTopicTDM[smallTopicTDM<0.01] = 0
  smallTopicTDM = smallTopicTDM*100
  
  
  smallTopicMatrix <- as.matrix(smallTopicTDM)
  #smallTopicMatrix[smallTopicMatrix>=0.008] <- 1
  #smallTopicMatrix[smallTopicMatrix<0.008] <- 0
  
  smallTopicMatrix <-  t(smallTopicMatrix) %*% smallTopicMatrix
  #smallTopicMatrix2 <-  smallTopicMatrix %*% t(smallTopicMatrix)
  
  ## Make Network
  net <- network(smallTopicMatrix, directed=FALSE)
  net %v% "mode" <- ifelse(betweenness(net)>mean(betweenness(net)), "big", "small")
  col = c("small" = "grey", "big" = "gold")
  ggnet2(net, mode = "kamadakawai", label=TRUE, color = "mode", palette = col, size = "degree")
}



