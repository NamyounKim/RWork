#############################
##### LDA Configuration #####
#############################

#Write Query for importing data(TextPrism Result)

#inputQuery <- 'select tp.crawl_data_id, tp.role, tp.keyword, tp.ranking from trendtracker.t_tp_result_rank_adhoc tp inner join	(select distinct a.crawl_data_id, mt.media_type_name from t_analysis_result a inner join t_crawl_data cd on a.crawl_data_id = cd.crawl_data_id inner join t_media_detail md on md.media_detail_id= cd.media_detail_id inner join t_media_type mt on mt.media_type_id = md.media_type_id where a.project_id = \'3000\' and mt.media_type_name <>\'SNS\') ee on tp.crawl_data_id = ee.crawl_data_id where user = \'kma_de_new\';'

inputQuery <- 'SELECT b.crawl_data_id, b.keyword, b.ranking, b.role , a.URL
FROM trendtracker.t_analysis_result a 
JOIN trendtracker.t_tp_result_rank_adhoc b USING (crawl_data_id)
WHERE a.project_id in (\'20160421\', \'20160424\')
and a.URL not like \'http://twitter.com%\'
and a.cat_product in (\'CADENZA\', \'AZERA\', \'MAXIMA\', \'AVALON\', \'IMPALA\', \'CHRYSLER300\', \'LACROSSE\', \'TAURUS\', \'CC\')
AND b.user in (\'kma_de_yg\', \'kma_de_yg2\', \'kma_de_yg3\');'

name <- "noTwitter_yg_all"

#Sparse term ratio
sparse <- "0.997"

#Number of topic
k<- 20

#Visualization 
#visual <- TRUE
visual <- TRUE


#Manual Spam Check
MSC <- FALSE

#Number of term by topic
numTermByTopic <- 50
