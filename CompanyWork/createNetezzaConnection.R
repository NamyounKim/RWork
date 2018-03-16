library(RJDBC)
library(DBI)
library(rJava)

getNetezzaConnection = function(){
  drv = JDBC("org.netezza.Driver","/home/r_driver/nzjdbc3.jar")
  conn <- dbConnect(drv, "jdbc:netezza://10.129.30.37:5480/cdsdb",user = "AP35002337", password = "rlaskadbs!2")
  return(conn)
}

