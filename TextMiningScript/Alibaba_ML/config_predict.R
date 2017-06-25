##################################
##### Predict Configuration #####
##################################

#Select method to create DTM
#1 : Genernal DTM
#2 : TF-IDF DTM
#3 : This DTM is removed columns that have low tf-idf value.
dtmMethod <-1

sparseRatio <- 0.9999

inputQuery <- 'SELECT crawl_data_id, crawled_date, keyword, ranking as count, role 
FROM alibabadb.t_tp_result_rank_adhoc
WHERE user="aug" and role <> "$$";'

# oct : 33,726 -> 33,724/33,725 - 17.6min/15.2min
# sep : 35,082 -> 35,082/35,082 - 15.0min/13.3min
# aug : 29,537 -> 29,530/29,532 - 14.5min/9.2min

insertTable <- 'alibabadb.t_sentiment_result'