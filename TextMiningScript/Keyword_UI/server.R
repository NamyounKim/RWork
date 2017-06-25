
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("/home/ruser/TextPrism/RSource/ML_functions.R")
require(shiny)
require(dplyr)
require(stringi)
require(tm)
require(reshape)
require(slam)
require(SnowballC)
require(RODBC)
require(RODBCext)
require(igraph)
require(network)
require(sna)
require(ggplot2)
require(GGally)

makeCorpus <- function(inputQuery, stopTerm){
  
  start.time <- Sys.time()
  
  ############################################
  ## Read in TM results
  ############################################
  #DB Connection
  print("Connect DB")
  #conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')
  conn <- odbcConnect('ccaMart',uid='trendtracker',pwd='#tt1234')
  #conn <- odbcConnect('smaali',uid='namyun',pwd='dighdi12')
  #conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')
  print("Loading Data from DB")
  inputQuery = 'Select crawl_data_id, role, keyword, ranking from ccadb.t_tp_result_rank_lgu where user = \'comm_con_dev\';'
  tm<-sqlQuery(conn,inputQuery)
  odbcClose(conn)
  
  #Pre Processing
  tm$keyword <- gsub(" ", "#", tm$keyword)
  #tm$keyword <- replace(tm$keyword, (tm$keyword %in% "알파"), "알파고")
  #tm$keyword <- replace(tm$keyword, (tm$keyword %in% "알파고가"), "알파고")
  #tm$keyword <- replace(tm$keyword, (tm$keyword %in% "알파고와"), "알파고")
  #tm$keyword <- replace(tm$keyword, (tm$keyword %in% "이세돌과"), "이세돌")
  
  
  ## Stop Term ##
  disuse.term <- stri_split_fixed(stopTerm, ",")
  #disuse.term <- c("아다", "와의", "수로","서울","오후","어제","판후","디어","쓰다","금지","배포")
  disuse.role <- c("OL")
  tm <- tm[!(tm$role %in% disuse.role),]
  tm <- tm[!(tm$keyword %in% disuse.term[[1]]),]
  
  tmKeyword <- fn_tm_keys(tm)
  print(paste("Total Document :",nrow(tmKeyword)))

  
  ##Duplication Check
  dupCheck <- duplicated(tmKeyword[,2])
  tmKeyword <- tmKeyword[!dupCheck,]
  
  print(paste("Target Document :",nrow(tmKeyword)))
  
  end.time <- Sys.time()
  runtime <- end.time - start.time
  print("DB Loading Time: ")
  print(runtime)
  return(tmKeyword)
}

makeDtm <- function(tmKeyword, sparse){
  corp<-Corpus(DataframeSource(tmKeyword))
  dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
  dtm <- removeSparseTerms(dtm, as.numeric(sparse))
  
  return(dtm)
}


makeAssMat <- function(dtm, mainTerm, corlimit){
  ## Create asscoiation keyword ##
  mTerm <- stri_split_fixed(mainTerm, ",")
  mTerm <- mTerm[[1]]
  assDf <- NULL
  
  for(i in 1:length(mTerm)){
    print(mTerm[i])
    at <-findAssocs(dtm, mTerm[i], corlimit)
    tempTerm <- mTerm[i]
    
    atDf <- data.frame(at[1])
    weight <- atDf[1]
    term <- rownames(atDf)
    temp <- cbind(weight,term)
    
    if(is.null(assDf)){
      assDf <- temp
    }
    else{
      assDf <- merge(assDf, temp, by.x="term", by.y="term", all.x = TRUE, all.y = TRUE)
      assDf <- replace(assDf, is.na(assDf), 0)
      rm(temp)
    }
  }
    
    ## Convert a Data Frame to a Numeric Matrix  ##
    new_dtmMatrix <- as.matrix(assDf[,2:ncol(assDf)])
    rownames(new_dtmMatrix)<- as.character(assDf[,1])
    new_dtmMatrix[new_dtmMatrix>=0.02] <- 1
    new_dtmMatrix[new_dtmMatrix<0.02] <- 0
    
    return(new_dtmMatrix)
}

makeNetwork <- function(new_dtmMatrix){
  
}



shinyServer(function(input, output) {
  
  getNetwork <- reactive({
    if(input$inputSql != 'Insert SQL!!'){
      withProgress({
        setProgress(message = "Loading corpus...")
        corpusResult <- makeCorpus(input$inputSql, input$stopTerm)
        
        setProgress(message = "Create DTM...")
        dtm <- makeDtm(corpusResult, input$sparse)
        
        setProgress(message = "Create Network Matrix...")
        new_dtmMatrix <- makeAssMat(dtm, input$mainTerm, input$corlimit)
        
        setProgress(message = "Create Network Graph...")
        net <- network(new_dtmMatrix, directed=FALSE)
        net %v% "mode" <- ifelse(betweenness(net)>mean(betweenness(net)), "big", "small")
        col = c("small" = "grey", "big" = "gold")
        network <- ggnet2(net,mode = "kamadakawai", label=TRUE, color = "mode", palette = col)
        #network <- ggnet2(net,mode = "kamadakawai", label=TRUE)
      })
    }else{
      network <- NULL
    }
    return(network)
  })
  
  output$network <- renderPlot({
    getNetwork()
  })

})
