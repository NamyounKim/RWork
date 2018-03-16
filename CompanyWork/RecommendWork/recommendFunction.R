library(dplyr)

# Make Sub Data Set
makeSubDataSet = function(data, grpCols, categoryNm){
  if(categoryNm==""){
    output = data %>% group_by_(.dots = grpCols) %>% summarise(pv=n())
  }else{
    temp = subset(data, prd_ltyp_nm_n == categoryNm)
    print(nrow(temp))
    output = temp %>% group_by_(.dots = grpCols) %>% summarise(pv=n())
  }
}

# Sampling & Get Product Vector
makeSpvm <- function(data, castVar ,sampleSize){
  if(sampleSize < 0){
    spvm = dcast(data, sid ~ castVar, sum, value.var = "pv")
  }else{
    sessionList = data %>% distinct(sid) %>% select(sid)
    targetSession = sessionList[sample(1:nrow(sessionList), sampleSize, replace = FALSE),]
    train_sub_product = data[data$sid %in% targetSession$sid, ]
    spvm = dcast(train_sub_product, sid ~ castVar, sum, value.var = "pv")
  }
}

# Convert to Rating Matrix (Product Vector, Minimum sum of col, Minimum sum of row)
makeRatingMatrix <- function(inputMat, keyCol, colLimit = 20, rowLimit = 4){
  inputMat = as.data.frame(inputMat)
  
  keyCol_Index = grep(keyCol, colnames(inputMat))
  
  cpm_target = inputMat %>% select_(-keyCol_Index)
  cpm_target = cpm_target[, colSums(cpm_target) > colLimit]
  targetColName = colnames(cpm_target)
  cpm_target = cpm_target[rowSums(cpm_target) > rowLimit, ]
  cpm_target[cpm_target == 0] = NA
  cpm_target[cpm_target > 10] = 10
  cpm_target[cpm_target < 0] = 0
  cpm_target = as.matrix(cpm_target)
  cpm_target = as(cpm_target, "realRatingMatrix")
}

# Get Product Sim# Convert to Rating Matrix (Product Vector, Minimum sum of col, Minimum sum of row)
makeSimilarity <- function(target, categoryName){
  ## Recommender Modeling ##
  recoModel = Recommender(target, method="IBCF", param=list(method="Jaccard", alpha = 0.1))
  
  ## Print Item Simility ##
  itemMat = as(getModel(recoModel)$sim, "matrix")
  itemMatDf = as.data.frame(itemMat)
  itemMatDf$pid = rownames(itemMatDf)
  itemSim = melt(itemMatDf, id=c("pid"))
  itemSim = subset(itemSim, itemSim$value > 0)
  
  write.csv(itemSim, paste0(categoryName,"_itemSim.csv"), row.names = FALSE)
}
