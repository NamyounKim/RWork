library(dplyr)
library(stringi)
library(reshape2)
library(data.table)
library(ggplot2)

options(java.parameters = "-Xmx20g")
source("~/rwork/createImpalaConnection.R")
source("~/rwork/createNetezzaConnection.R")
netCon = getNetezzaConnection()
connImpala = getImpalaConnection()

# AP몰 고객 가져오기
query="
SELECT d.onl_site_cd,
d.comcsno,
d.user_id,
d.user_nm,
d.sex_cd,
d.birthday,
d.age_cd,
d.mbr_grd_cd,
d.bad_mbr_yn,
d.pchrg_mbr_yn
FROM bdmdb.d_conl_site_user d
WHERE d.onl_site_cd = 'CMC'
;"
raw_cus_info = dbGetQuery(connImpala, query)
raw_cus_info = raw_cus_info[!duplicated(raw_cus_info$comcsno),]
raw_cus_info$age_cd[is.na(raw_cus_info$age_cd)] = 0
rm(raw_review)

# 연령대 만들기
makeAgeRange = function(x){
  if(x >= 10 & x<20){
    ageRange = "10대"
  }else if(x >= 20 & x<30){
    ageRange = "20대"
  }else if(x >= 30 & x<40){
    ageRange = "30대"
  }else if(x >= 40 & x<50){
    ageRange = "40대"
  }else if(x >= 50 & x<60){
    ageRange = "50대"
  }else if(x >= 60 & x<70){
    ageRange = "60대"
  }else if(x >= 70 & x<80){
    ageRange = "70대"
  }else if(x >= 80 & x<90){
    ageRange = "80대"
  }else if(x >= 90 & x<100){
    ageRange = "90대"
  }else{
    ageRange = "etc"
  }
}
raw_cus_info$ageRange = sapply(as.numeric(raw_cus_info$age_cd), makeAgeRange)


# AP몰 고객 구매이력
query="SELECT 
COMCSNO
,SALDT
,NEWCEMPRDNM
,BRANDNM
,BEF_SALAMT
,BEF_DCAMT
,BEF_NETAMT
FROM CDMDB.CDWDBA.V_KCEM_COMCS_SALINFO
WHERE SALDT >= 20171101
AND BEF_NETAMT > 0
AND CHCD = '031'
;"
raw_apmall_salinfo = dbGetQuery(netCon, query)
raw_apmall_salinfo = raw_apmall_salinfo %>% mutate(salYear = substr(SALDT,1,4)
                                                   ,salMonth = substr(SALDT,5,6)
                                                   ,salDay = substr(SALDT,7,8))
raw_apmall_salinfo = raw_apmall_salinfo %>% mutate(purDate = as.Date(paste0(salYear, "-",salMonth, "-",salDay)))
raw_apmall_salinfo90 = raw_apmall_salinfo %>% filter(difftime(as.Date(Sys.time()), raw_apmall_salinfo$purDate, units = "day")<90)

raw_apmall_salinfo90 = as.data.table(raw_apmall_salinfo90)
head(raw_apmall_salinfo90)
cpm = dcast.data.table(raw_apmall_salinfo90, COMCSNO ~ NEWCEMPRDNM, length)

# 각 제품별 sparseRatio 구하기
sparseRatio = apply(cpm[,2:ncol(cpm)], 2, FUN = function(x){length(which(x==0)) / nrow(cpm)})
boxplot(sparseRatio)



prdSalStat = data.frame(prdNm = colnames(cpm)[2:ncol(cpm)]
                        ,colMeanVal = colMeans(cpm[,2:ncol(cpm)])
                        ,colVarVal = apply(cpm[,2:ncol(cpm)], 2, sd)
                        )

ggplot(prdSalStat %>% filter(colMeanVal<= 0.05, colVarVal <= 0.3), aes(x=colMeanVal, y=colVarVal, label=prdNm))+geom_text()

colMeanVal = colMeans(cpm[,2:ncol(cpm)])
colVarVal = apply(cpm[,2:ncol(cpm)], 2, sd)


# 1회성 구매자 추출
temp = rowSums(cpm[,2:ncol(cpm)])

quantile(temp, probs = seq(0,1,0.1))

plot(colMeanVal, colVarVal)

