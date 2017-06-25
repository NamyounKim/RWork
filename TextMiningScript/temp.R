require(ggplot2)
conn <- odbcConnect('ccaMart',uid='trendtracker',pwd='#tt1234')
getContent <- 'SELECT crawl_data_id, published_date, author, title, url FROM ccadb.t_solr_data where who_did_this=\'community\' OR who_did_this=\'comm_dev\''
content <- sqlQuery(conn,getContent)
odbcClose(conn)

for(i in 1:nrow(content)){
  if(unlist(gregexpr(pattern=".com", content$url[i])) > 0){
    content$site[i] = substr(content$url[i], 1, unlist(gregexpr(pattern=".com", content$url[i]))-1)
  }
  else if(unlist(gregexpr(pattern=".net", content$url[i])) > 0){
    content$site[i] = substr(content$url[i], 1, unlist(gregexpr(pattern=".net", content$url[i]))-1)
  }
  else if(unlist(gregexpr(pattern=".co.kr", content$url[i])) > 0){
    content$site[i] = substr(content$url[i], 1, unlist(gregexpr(pattern=".co.kr", content$url[i]))-1)
  }

  content$site[i] <- gsub("http://", "", content$site[i])
}

#사이트별 문서 건수
content_gb = content %>% group_by(site) %>% summarise(count=n())
content_gb = filter(content_gb, !(site %in% c('bbs.mi','www.etimes','news.mk','twitter','www.ezday')))
ggplot(content_gb, aes(x=reorder(site,-count), y=count)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))

dd_sub1 = dd %>% group_by(site) %>% summarise(count=n())
dd_sub1 = dd_sub1[order(-dd_sub1$count),]
ggplot(dd_sub1, aes(x=reorder(site, -count), y=count)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=11))

#================================================================================================
topicNum = 3
dd = subset(id_topic, doc_topic==topicNum)
dd = dd[order(-dd$maxProb),]
head(dd, 20)
dd = merge(id_topic, content, by="crawl_data_id", all.x = TRUE)



#사이트별 문서 건수
dd_gb = dd %>% group_by(site) %>% summarise(count=n())
ggplot(dd_gb, aes(x=site, y=count)) + geom_bar(stat = "identity") + ggtitle(topicNum)

#작성자별 문서 건수
dd_gb2 = dd %>% group_by(author) %>% summarise(count=n())
ggplot(dd_gb2, aes(x=author, y=count)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(topicNum)



#=============================================================================
  library(FactoMineR)

mds.res <- dist(phi)
fit <- princomp(phi, cor=TRUE)
fit <- PCA(phi)

temp <- dist(phi, method="canberra")
temp = cmdscale(temp,eig=TRUE, k=2)

dx = temp$points[,1]
dy = temp$points[,2]
plot(dx,dy,xlab="Coordinate 1", ylab="Coordinate 2", main="TOPIC MAP",	type="n")
text(dx, dy, labels = row.names(phi), cex=.7)


svd_tsne <- function(x) tsne(svd(x)$u)

svd_tsne(phi)

svd_result = svd(phi)

temp2 = distance(phi, "unifrac")

# Topic Map

ggplot(coord, aes(x=V1, y=V2))+geom_point(shape=5) + geom_text(aes(label=row.names(coord)),hjust=0, vjust=0)
