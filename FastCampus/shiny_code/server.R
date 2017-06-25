require(shiny)
require(dplyr)
require(stringi)
require(tm)
require(reshape)
require(slam)
require(SnowballC)
require(igraph)
require(network)
require(sna)
require(ggplot2)
require(GGally)
library(readr)

function(input, output) {
  
  
  output$network <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    parsedData = read_csv(inFile$datapath)
    
    output$text <- renderText({
      paste0("The number of document is: ", nrow(parsedData))
    })
    
    parsedData$parsedContent = gsub(" ","  ",parsedData$parsedContent)
    
    corp = VCorpus(VectorSource(parsedData$parsedContent))
    corp = tm_map(corp, removePunctuation)
    
    if(nchar(stri_split_fixed(input$stopTerm, ",")) > 0 ){
      disuse.term <- unlist(stri_split_fixed(input$stopTerm, ","))
      corp <- tm_map(corp, removeWords, disuse.term)
    }
    corp <- tm_map(corp, PlainTextDocument)
    
    #Document Term Matrix 생성
    dtmW<-DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, 
                                                wordLengths=c(2,Inf), 
                                                weighting = function(x) weightTfIdf(x, normalize = TRUE)))
    
    ## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
    colnames(dtmW) = trimws(colnames(dtmW))
    dtmW = dtmW[,nchar(colnames(dtmW)) > 1]
    
    dtmW <- removeSparseTerms(dtmW, as.numeric(input$sparse))
    
    #매트릭스 크기 조절하기
    dtmW_m = as.matrix(dtmW)
    cor_termW = cor(dtmW_m)
    cor_termW[cor_termW < input$corLimit] = 0
    
    ## Make Network
    net <- network(cor_termW, directed=FALSE)
    net %v% "mode" <- ifelse(betweenness(net) > quantile(betweenness(net), 0.9), "big", "small")
    col = c("small" = "grey", "big" = "gold")
    set.edge.value(net, "edgeSize", cor_termW*2)
    
    ggnet2(net, label=TRUE, label.size = 5, color = "mode", 
           palette = col, size = "degree", edge.size = "edgeSize",
           family="AppleGothic")
  })
}