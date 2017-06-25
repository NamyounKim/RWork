options(shiny.maxRequestSize=30*1024^2)
source("/home/ruser/TextPrism/RSource/ML_functions.R")
source("/home/ruser/TextPrism/RSource/createNamJson.R")


require(dplyr)
require(stringi)
require(tm)
require(reshape)
require(slam)
require(SnowballC)
require(topicmodels)
require(RODBC)
require(RODBCext)
require(servr)
require(LDAvis)
require(igraph)
require(network)
require(sna)
require(ggplot2)
require(GGally)
require(doParallel)

registerDoParallel()

makeCorpus <- function(inputQuery, MSC){
  
  start.time <- Sys.time()
  
  ############################################
  ## Read in TM results
  ############################################
  #DB Connection
  print("Connect DB")
  #conn <- odbcConnect('SmartSMA_Operating',uid='trendtracker',pwd='#tt1234')
  #conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')
  conn <- odbcConnect('ccaMart',uid='trendtracker',pwd='#tt1234')
  #conn <- odbcConnect('smaali',uid='namyun',pwd='dighdi12')
  #conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')
  
  print("Loading Data from DB")
  #select crawl_data_id, keyword, role, ranking from t_tp_result_rank_adhoc where user = "navien";
  
  tm<-sqlQuery(conn,inputQuery)
  odbcClose(conn)
  
  #disuse.role <-c("hash","NNBJKS","SN","MAJ","UNKNOWN","MAG","VCPEC","url","SL")
  disuse.role <-c("$$")
  
  disuse.term <- c("하다","되다","있다","같다","좋다","자형","point","네요","the","one","pc","nc","of","to","for","in","ess","포항","청약","무주택자","vs","or"
                   ,"mm","입니다","gg","으십니다","셔서","라는","모바","비스","mr","xl","이야","eq","se","rt","bt","be","ea","on")
  
  #disuse.term <- c("nm","go","year",".no","use","autoguide.com","dx","tt","news.more","rm","$$","autocar","eos","nx","mall","st.","@media","div_*","ps","bloomberg","20ps")
  
  tm$keyword <- gsub(" ", "_", tm$keyword)
  tm <- tm[!(tm$keyword %in% disuse.term),]
  tm <- tm[!(tm$role %in% disuse.role),]
  tm <- tm[length(tm$keyword)>1,]
  
  tmKeyword <- fn_tm_keys(tm)
  print(paste("Total Document :",nrow(tmKeyword)))
  
  ####################################
  ## Manual Spam Check
  ####################################
  if(MSC){
    #spamDocId <- read.table(file="./spamDocId.txt", header=TRUE)
    spamDocId <- read.csv(file = "./output/newly_995_20DOC_LDA_Result.csv")
    spamDocId <- subset(spamDocId, !(doc_topic %in% c(1,4,5,9,10,11,12,14,15,17,19,20)))
    spamCheck <- tmKeyword$crawl_data_id %in% spamDocId$crawl_data_id
    tmKeyword <- tmKeyword[!spamCheck,]
  }
  
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

makeCorpusFromFile <- function(tm){
  
  start.time <- Sys.time()
  
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


ldaAnalysis <- function(tmKeyword, sparse, name, k, numTermByTopic, visual){
  
  start.time <- Sys.time()
  
  sparseRe <- gsub("0.","_",sparse)
  
  corp<-Corpus(DataframeSource(tmKeyword), readerControl=list(language="ko"))
  dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
  dtm <- removeSparseTerms(dtm, as.numeric(sparse))
  
  ##Remove low tf-idf col and row
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  new_dtm <-dtm[,term_tfidf >= 0.08]
  new_dtm <-new_dtm[row_sums(new_dtm)>0,]
  
  ############################################
  ## Running LDA
  ############################################
  print("Start LDA")
  SEED <-2010
  
  lda_tm <- LDA(new_dtm, control=list(seed=SEED), k=as.numeric(k))
  
  doc_topic <- topics(lda_tm,1)
  
  
  ############################################
  ### Call makeVisualization
  ############################################
  makeVisualization(visual, lda_tm, doc_topic, corp, new_dtm, name, sparseRe, k)
  
  end.time <- Sys.time()
  runtime <- end.time - start.time
  print("Processing Time: ")
  print(runtime)
  
  return(lda_tm)
}

makeLdaOutput <- function(lda_tm, sparse, name, k, numTermByTopic, visual){
  
  start.time <- Sys.time()
  
  sparseRe <- gsub("0.","_",sparse)
  
  term_topic <- terms(lda_tm, numTermByTopic)
  
  write.table(term_topic, paste("./output/",name,sparseRe,"_",k,"_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)
  
  return(term_topic)
}

makeLdaDocOutput <- function(lda_tm, sparse, name, k, corpusResult){
  
  start.time <- Sys.time()
  
  sparseRe <- gsub("0.","_",sparse)
  
  ############################################
  ## Make Doc Topic File
  ############################################
  #Doc_topic result making
  doc_topic <- topics(lda_tm,1)
  doc_topic_df <- as.data.frame(doc_topic)
  doc_topic_df$rown <- as.numeric(row.names(doc_topic_df))
  corpusResult$rown <- as.numeric(row.names(corpusResult))
  
  #doc prob make
  docProb <- posterior(lda_tm)$topics %>% as.matrix
  docProb_df <- as.data.frame(docProb)
  docProb_df$rown <- as.numeric(row.names(docProb_df))
  max<-NULL
  for(i in 1:nrow(docProb_df)){
    max<-rbind(max,max(docProb[i,]))
  }
  docProb_df$maxProb<-max
  
  id_topic <- merge(doc_topic_df, docProb_df, by="rown")
  id_topic <- merge(id_topic, corpusResult, by="rown")
  id_topic <- subset(id_topic,select=c("rown","doc_topic","crawl_data_id","maxProb"))
  
  write.table(id_topic, paste("./output/",name,sparseRe,"_",k,"DOC_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)
  
  return(id_topic)
}




makeVisualization <- function(visual, lda_tm, doc_topic, corp, new_dtm, name, sparseRe, k){
  #########################################
  ## Make visualization
  #########################################
  if(visual){
    # phi is probabilities of the topics for each of the terms
    phi <- posterior(lda_tm)$terms %>% as.matrix
    
    # theta is probabilities of the topics for each the document
    theta <- posterior(lda_tm)$topics %>% as.matrix
    #phi2 <- phi[ ,order(as.integer(colnames(phi)))]
    vocab <- colnames(phi)
    
    doc_length <- vector()
    doc_topic_df<-as.data.frame(doc_topic)
    
    #get document length
    for( i in as.numeric(row.names(doc_topic_df))){
      temp <- corp[[i]]$content
      doc_length <- c(doc_length, nchar(temp[2]))
    }
    
    temp_frequency <- as.matrix(new_dtm)
    
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    
    json_lda <- createNamJson(phi = phi, theta = theta,
                              vocab = vocab,
                              doc.length = doc_length,
                              term.frequency = freq_matrix$Freq,
                              mds.method = canberraPCA)
    
    #serVis(json_lda, out.dir = paste("/home/ruser/TextPrism/LDAvis_Result/",name,sparseRe,"_",k,sep=""), open.browser = FALSE)
    ##release to TOMCAT
    serVis(json_lda, out.dir = paste("/data001/tomcat/webapps/",name,sparseRe,"_",k,sep=""), open.browser = FALSE)
  }
}

makeNetwork <- function(networkMatrix){
  g <- graph.adjacency(networkMatrix, weighted=T, mode = "undirected")
  g <- simplify(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  
  set.seed(3952)
  layout1 <- layout.kamada.kawai(g)
  layout2 <- layout.graphopt(g)
  plot.igraph(g, layout=layout1,edge.width=2, main="TopicKeword Network", vertex.size=10*sqrt(hub.score(g)$vector))
  
  #write.graph(g,file ="test.ncol",format="ncol")
}


#####################################################################################
shinyServer(function(input, output, session) {
  
  getCorpus <- reactive({
    if(input$inputType == 'db'){ ##Input Type check!
      if(input$inputSql != 'Insert SQL!!'){
        isolate({
          withProgress({
            setProgress(message = "Loading corpus...")
            corpusResult <- makeCorpus(input$inputSql, input$MSC)
          })
        })
      }else{
        corpusResult <- NULL
      }
    }else{
      if(!is.null(input$inputFile)){
        inFile<-input$inputFile
        tm <- read.csv(inFile$datapath, header=TRUE, sep=",")
        isolate({
          withProgress({
            corpusResult <- makeCorpusFromFile(tm)
          })
        })
      }else{
        if(nrow(tm)>1){
          corpusResult <- makeCorpusFromFile(tm)
        }else{
          corpusResult <- NULL
        }
        
      }
    }
    return(corpusResult)
  })
  
  getLdaResult <- reactive({
    corpusResult <- getCorpus()
    if(!is.null(corpusResult)){
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          lda_tm <- ldaAnalysis(corpusResult, input$sparse, input$projectName, input$clusters, input$termCount, input$visual)
          
        })
      })
    }else{
      lda_tm <- NULL
    }
    
  })
  
  getLdaOutput <- reactive({
    lda_tm <- getLdaResult()
    if(!is.null(lda_tm)){
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          term_topic <- makeLdaOutput(lda_tm, input$sparse, input$projectName, input$clusters, input$termCount, input$visual)
        })
      })
    }
  })
  
  getLdaDocOutput <- reactive({
    lda_tm <- getLdaResult()
    if(!is.null(lda_tm)){
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          corpusResult <- getCorpus()
          term_doc <- makeLdaDocOutput(lda_tm, input$sparse, input$projectName, input$clusters,corpusResult)
        })
      })
    }
  })

  
  getTdm <- reactive({
    ##topicTDM
    lda_tm <- getLdaResult()
    if(!is.null(lda_tm)){
      topicTDM <- posterior(lda_tm)$term
      
      smallTopicTDM <- topicTDM[,colSums(topicTDM)>0.008]
      
      smallTopicMatrix <- as.matrix(smallTopicTDM)
      smallTopicMatrix[smallTopicMatrix>=0.008] <- 1
      smallTopicMatrix[smallTopicMatrix<0.008] <- 0
      
      smallTopicMatrix <-  t(smallTopicMatrix) %*% smallTopicMatrix
      #smallTopicMatrix2 <-  smallTopicMatrix %*% t(smallTopicMatrix)
      
      ## Make Network
      net <- network(smallTopicMatrix, directed=FALSE)
      net %v% "mode" <- ifelse(betweenness(net)>mean(betweenness(net)), "big", "small")
      col = c("small" = "grey", "big" = "gold")
      ggnet2(net, mode = "kamadakawai", label=TRUE, color = "mode", palette = col)
      #ggnet2(net, mode = "kamadakawai", label=TRUE)
    }
  })
  
  #################
  #### UI Part ####
  #################
  output$console <- renderPrint({
    count <- getLdaResult()
  })
  
  output$visual <- renderPrint({
    if(input$inputSql != 'Insert SQL!!'){
      if(input$visual){
        sparseRe <- gsub("0.","_",input$sparse)
        ldaResultURL <- cat(paste("http://165.243.188.249:8080/",input$projectName,sparseRe,"_",input$clusters,sep=""))
      }
      else{
        ldaResultURL <- cat("Visualization wasn't selected.")
      }
      
    }
    else{
      ldaResultURL <- cat("Display visualization URL after processing.")
    }

  })
  
  output$lda <- renderTable({
    getLdaOutput()
  })
  
  output$network <- renderPlot({
    getTdm()
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      #paste('data-', Sys.Date(), '.csv', sep='')
      sparseRe <- gsub("0.","_",input$sparse)
      paste(input$projectName,sparseRe,"_",input$clusters,"_LDA_Result.csv",sep="")
      },
    
    content = function(file) {
      write.csv(getLdaOutput(), file , row.names = FALSE)
      }
    )
  
  output$downloadData2 <- downloadHandler(
    
    filename = function() {
      #paste('data-', Sys.Date(), '.csv', sep='')
      sparseRe <- gsub("0.","_",input$sparse)
      paste(input$projectName,sparseRe,"_",input$clusters,"DOC_LDA_Result.csv",sep="")
    },
    
    content = function(file) {
      write.csv(getLdaDocOutput(), file, row.names =FALSE )
    }
  )
})
