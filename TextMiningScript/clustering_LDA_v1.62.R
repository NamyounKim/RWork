rm(list=ls())
gc()

start.time <- Sys.time()
source("/home/ruser/TextPrism/RSource/ML_functions.R")
source("/home/ruser/TextPrism/RSource/createNamJson.R")
source("/home/ruser/TextPrism/RSource/config_LDA.R")
source("/home/ruser/TextPrism/RSource/network_v1.1.R")


#package check & install & load
libraryList <- c("dplyr","stringi","tm","caret","reshape","lsa","RODBC","RODBCext","topicmodels","servr","LDAvis")

for(lib in libraryList){
  package.checking <- find.package(lib,quiet=TRUE)
  if(length(package.checking) == 0){
    install.packages(lib)
  }
}
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
require(doParallel)

registerDoParallel()

sparseRe <- gsub("0.","_",sparse)
print(visual)
print(name)
print(paste("Sparse Term Ratio :",sparse))
print(paste("Number of Cluster:",k))
print(Sys.time())



############################################
## Read in TM results
############################################

#DB Connection
print("Connect DB")
#conn <- odbcConnect('ccaMart',uid='trendtracker',pwd='#tt1234')
conn <- odbcConnect('smartSMA_Development_New',uid='trendtracker',pwd='#tt1234')
#conn <- odbcConnect('Alibaba',uid='trendtracker',pwd='#tt1234')
print("Loading Data from DB")

tm<-sqlQuery(conn,inputQuery)
odbcClose(conn)

#tm <- read.csv(file="/home/ruser/TextPrism/input/out_G5_youtube_dbinsert.csv")
tm <- subset(tm,as.integer(tm$ranking)>0)

disuse.role <-c("hash","NNBJKS","SN","MAJ","UNKNOWN","MAG","VCPEC","url","SL","$$")
#disuse.role <-c("$$")

#disuse.term <- c("하다","되다","있다","같다","좋다","자형","point","네요","the","one","pc","nc","of","to","for","in","ess","포항","청약","무주택자","vs","or"
#                ,"mm","입니다","gg","으십니다","셔서","라는","모바","비스","mr","xl","이야","eq","se","rt","bt","be","ea","on","from","cv","nbsp","ed","eb","ac","bc")
disuse.term <- c("nm","go","year",".no","use","autoguide.com","dx","tt","news.more","rm","$$","autocar","eos","nx","mall","st.","@media","div_*","ps","bloomberg","20ps")

tm$keyword <- gsub(" ", "#", tm$keyword)
tm <- tm[!(tm$keyword %in% disuse.term),]
tm <- tm[!(tm$role %in% disuse.role),]
tm <- tm[length(tm$keyword)>1,]


## TM results into document keywords matrices
print("Make DTM")
tmKeyword <- fn_tm_keys(tm)
print(paste("Total Document :",nrow(tmKeyword)))

####################################
## Manual Spam Check
####################################
if(MSC){
  spamDocId <- read.table(file="./TextPrism/spamDocId.txt", header=TRUE)
  spamCheck <- tmKeyword$crawl_data_id %in% spamDocId$spamDocId
  tmKeyword <- tmKeyword[!spamCheck,]
}

##Duplication Check
dupCheck <- duplicated(tmKeyword[,2])
tmKeyword <- tmKeyword[!dupCheck,]

print(paste("Target Document :",nrow(tmKeyword)))
corp<-Corpus(DataframeSource(tmKeyword))
dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))
dtm <- removeSparseTerms(dtm, as.numeric(sparse))

##Remove low tf-idf col and row
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
new_dtm <-dtm[,term_tfidf >= 0.1]
new_dtm <-new_dtm[row_sums(new_dtm)>0,]

############################################
## Running LDA
############################################
print("Start LDA")
SEED <-2010

lda_tm <- LDA(new_dtm, control=list(seed=SEED), k=as.numeric(k))

doc_topic <- topics(lda_tm,1)
term_topic <- terms(lda_tm, numTermByTopic)


write.table(term_topic, paste("/home/ruser/TextPrism/output/",name,sparseRe,"_",k,"_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)


#Doc_topic result making
doc_topic_df <- as.data.frame(doc_topic)
doc_topic_df$rown <- as.numeric(row.names(doc_topic_df))
tmKeyword$rown <- as.numeric(row.names(tmKeyword))

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
id_topic <- merge(id_topic, tmKeyword, by="rown")
id_topic <- subset(id_topic,select=c("rown","doc_topic","crawl_data_id","maxProb"))

write.table(id_topic, paste("/home/ruser/TextPrism/output/",name,sparseRe,"_",k,"_raw","_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)

outPutForQV <- fn_LDA_Result_for_QV(term_topic)
write.table(outPutForQV, paste("/home/ruser/TextPrism/output/",name,sparseRe,"_",k,"_QV","_LDA_Result.csv",sep=""),sep=",", row.names=FALSE)

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
end.time <- Sys.time()
svmtraintime <- end.time - start.time

print("Analysis Time:")
print(svmtraintime)
print("LDA Complete")

rm(tm)
rm(start.time)
rm(end.time)

save.image(file=paste("/home/ruser/TextPrism/output/",name,sparseRe,"_",k,"_clustering_LDA_Result.RData",sep=""))
print(paste("http://165.243.188.249:8080/",name,sparseRe,"_",k))
