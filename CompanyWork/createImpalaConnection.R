system("/home/AP35002337/bd_kerberos.sh")

library(RJDBC)
library(DBI)
library(rJava)

getImpalaConnection = function(){
  hive.class.path = list.files(path = c("/opt/cloudera/parcels/CDH/lib/hive/lib"), pattern = "jar", full.names = T)
  impala.class.path = list.files(path = c("/home/drivers/r_jdbc"), pattern = "jar", full.names = T)
  hadoop.lib.path = list.files(path = c("/opt/cloudera/parcels/CDH/lib/hadoop/lib"), pattern = "jar", full.names = T)
  hadoop.class.path = list.files(path = c("/opt/cloudera/parcels/CDH/lib/hadoop"), pattern = "jar", full.names = T)
  class.path = c(hive.class.path, hadoop.lib.path, hadoop.class.path, impala.class.path)
  
  .jinit(classpath = class.path, parameters = "-Djava.security.krb5.conf=/etc/krb5.conf")
  
  drv <- JDBC("com.cloudera.impala.jdbc4.Driver")
  conn <- dbConnect(drv, "jdbc:impala://aplhadoop05:21050;AuthMech=1;KrbRealm=DS.AMOREPACIFIC.COM;KrbHostFQDN=aplhadoop05;KrbServiceName=impala")
  
  return(conn)
}

