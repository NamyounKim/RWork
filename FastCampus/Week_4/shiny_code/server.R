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

  output$networkPlot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile = input$inputFile
    
    if (is.null(inFile))
      return(NULL)
    parsedData = read_csv(inFile$datapath)
    
    output$displayRow <- renderText({
      paste0("The number of document is: ", nrow(parsedData))
    })
    
    #동의어 / 불용어 사전 불러오기
    stopWordDic = read_csv("./stopword_ko.csv")

    parsedData$parsedContent = gsub(" ","  ",parsedData$parsedContent)
    
    corp = VCorpus(VectorSource(parsedData$parsedContent))
    corp = tm_map(corp, removePunctuation)
    
    # 불용어 사전 단어 우선 제거
    corp = tm_map(corp, removeWords, stopWordDic$stopword)
    
    # 사용자가 직접 지정한 단어 제거
    if(nchar(stri_split_fixed(input$stopTerm, ",")) > 0 ){
      disuse.term = unlist(stri_split_fixed(input$stopTerm, ","))
      corp = tm_map(corp, removeWords, disuse.term)
    }
    corp = tm_map(corp, PlainTextDocument)
    
    #Document Term Matrix 생성
    dtmW = DocumentTermMatrix(corp, control=list(removeNumbers=T
                                                ,wordLengths=c(2,Inf)
                                                #,weighting = function(x) weightTfIdf(x, normalize = TRUE)
                                                )
                              )
    
    ## 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
    colnames(dtmW) = trimws(colnames(dtmW))
    dtmW = dtmW[,nchar(colnames(dtmW)) > 1]
    
    dtmW = removeSparseTerms(dtmW, as.numeric(input$sparse))
    
    #매트릭스 크기 조절하기
    dtmW_m = as.matrix(dtmW)
    cor_termW = cor(dtmW_m)
    cor_termW[cor_termW < input$corLimit] = 0
    
    # 다른 노드와 연관성이 0인 노트 제거하기
    removeTarget = colSums(cor_termW) == 1
    cor_termW = cor_termW[!removeTarget, !removeTarget]
    
    ## Make Network
    net <- network(cor_termW, directed=FALSE)
    # betweenness값 상위 20% 이면서 eigenvector 값이 상위 10%이면 "High" -> 빨강색
    # betweenness값 상위 20% 이면서 eigenvector 값이 하위 90%이면 "Medium" -> 노란색
    # betweenness값 하위 80% 이면 "Low" -> 회색
    net %v% "mode" = ifelse(betweenness(net) > quantile(betweenness(net), 0.8)
                            ,ifelse(evcent(net) > quantile(evcent(net), 0.9),"High","Medium"), "Low")
    node_color = c("Low" = "grey", "Medium" = "darkgoldenrod1", "High"="brown1")
    
    set.edge.value(net, "edgeSize", cor_termW*2)
    
    ggnet2(net, label=TRUE, label.size = 4, color = "mode"
           ,palette = node_color, size = "degree"
           ,edge.size = "edgeSize"
           ,family="AppleGothic"
           )
  })
}