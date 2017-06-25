## TM_Keywords

fn_tm_keys <- function(tmResult){
 tm_keys <-
  tmResult %>% 
  group_by(crawl_data_id) %>%
  mutate(kw=stri_dup(stri_c(keyword,"  "),ranking)) %>%
  summarise(keywords=stri_c(kw,collapse="  "))
 return(tm_keys)
}

## TM_Roles

fn_tm_roles <- function(tmResult){
 tm_roles <-
  tmResult %>%
  group_by(crawl_data_id, role) %>%
  summarise(sumCount=sum(ranking))
 
 tm_roles <- cast(tm_roles, crawl_data_id ~ role, sum)
 tm_roles <- subset(tm_roles, select=c(crawl_data_id,A1,A2,AJ,AZ,NB,NN, NZ,url,VB))
 return(tm_roles)
}

## TM_Doc_Type
fn_tm_target <- function(tmResult){
 tm_doc_type <- tmResult %>% group_by(crawl_data_id, target) %>% summarise(sumCount=sum(ranking))
 tm_doc_type <- subset(tm_doc_type, select=(-sumCount))
 return(tm_doc_type)
}

## Make DTM
fn_makeDTM <- function(tm_keys,sparseTerm){
 corp <- Corpus(DataframeSource(tm_keys))
 dtm <- DocumentTermMatrix(corp, 
                           control=list(removeNumbers=TRUE, 
                                        wordLengths=c(2,Inf)))
 dtm <- removeSparseTerms(dtm, sparseTerm)
 dtmDf <- as.data.frame(as.matrix(dtm))
 return(dtmDf)
}

## Make TF-IDF DTM
fn_makeTfIdfDTM <- function(tm_keys,sparseTerm){
 corp <- Corpus(DataframeSource(tm_keys))
 dtm <- DocumentTermMatrix(corp,
                           control=list(weighting=weightTfIdf,
                                        removeNumbers=TRUE,
                                        wordLengths=c(2,Inf)))
 dtm <- removeSparseTerms(dtm, sparseTerm)
 dtmDf <- as.data.frame(as.matrix(dtm))
 return(dtmDf)
}

## Make TF-IDF DTM2
##Remove low tf-idf col and row
fn_makeTfIdfDTM2 <- function(tm_keys, sparseTerm, i){
  corp <- Corpus(DataframeSource(tm_keys))
  dtm <- DocumentTermMatrix(corp, 
                            control=list(removeNumbers=TRUE, 
                                         wordLengths=c(2,Inf)))
  dtm <- removeSparseTerms(dtm, sparseTerm)
  
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  new_dtm <-dtm[,term_tfidf >= i]
  new_dtm <-new_dtm[row_sums(new_dtm)>0,]
  
  dtmDf <- as.data.frame(as.matrix(new_dtm))
  return(dtmDf)
}



##LDA_Result_change_for Qlikview
fn_LDA_Result_for_QV <- function(term_topic){
 temp <-NULL
 output<-NULL

 for(i in 1:ncol(term_topic)){
   for(j in 1:nrow(term_topic)){
    temp$topicNo <- i
    temp$keyword <- term_topic[j,i]
    output <-  rbind(output,temp)
    }
  }
 
 return(output)
}

##LDA_Result_change_for Qlikview
fn_LDA_term_Result_for_QV <- function(phi){
 temp <-NULL
 output<-NULL

 for(i in 1:ncol(phi)){
   for(j in 1:nrow(phi)){
    temp$topicNo <- j
    temp$keyword <- noquote(colnames(phi)[i])
    temp$termProb <- phi[j,i]
    output <-  rbind(output,temp)
    }
  }
 return(output)
}


##Build a Graph
makeNetwork <- function(networkMatrix){
  g <- network(networkMatrix, weighted=T, mode = "undirected")
  g <- simplify(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  
  set.seed(3952)
  layout1 <- layout.kamada.kawai(g)
  layout2 <- layout.graphopt(g)
  plot.igraph(g, layout=layout1,edge.width=2, main="TopicKeword Network", vertex.size=10*sqrt(hub.score(g)$vector))
  
  write.graph(g,file ="test.ncol",format="ncol")
}

