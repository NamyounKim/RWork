trainDataProcessing <- function(train){
  
  train$year <- as.factor(year(ymd_hms(train$datetime)))
  train$month <- as.factor(month(ymd_hms(train$datetime)))
  train$hour <- as.factor(hour(ymd_hms(train$datetime)))
  train$weekdays <- as.factor(weekdays(ymd_hms(train$datetime)))
  train$dayType <- as.factor(ifelse((train$workingday==0)&(train$holiday==0),1,
                                    ifelse((train$workingday==0)&(train$holiday==1),2,
                                           ifelse((train$workingday==1)&(train$holiday==0),3,4))))
  train$diffTemp <- train$temp - train$atemp
  train$logCount <- log(train$count)
  train$logCasual <- log(train$casual+1)
  train$logRegistered <- log(train$registered+1)
  
  train$tempRange <- ifelse(train$temp < 12.71, 1,
                             ifelse((train$temp >=12.71) & (train$temp < 19.27), 2,
                                    ifelse((train$temp >=19.27) & (train$temp < 22.55), 3,
                                           ifelse((train$temp >=22.55) & (train$temp < 27.47), 4,
                                                  ifelse((train$temp >=27.47) & (train$temp < 29.93), 5,
                                                         ifelse((train$temp >=29.93), 6,7))))))
  
  train$cTempRange <- ifelse(train$temp < 12.71, 1,
                            ifelse((train$temp >=12.71) & (train$temp < 15.71), 2,
                                   ifelse((train$temp >=15.71) & (train$temp < 19.27), 3,
                                          ifelse((train$temp >=19.27) & (train$temp < 23.37), 4,
                                                 ifelse((train$temp >=23.37) & (train$temp < 29.93), 5,
                                                        ifelse((train$temp >=29.93), 6,7))))))
                                          
  
  train$rTempRange <- ifelse(train$temp < 12.71, 1,
                             ifelse((train$temp >=12.71) & (train$temp < 19.27), 2,
                                    ifelse((train$temp >=19.27) & (train$temp < 22.55), 3,
                                           ifelse((train$temp >=22.55) & (train$temp < 29.93), 4,
                                                  ifelse((train$temp >=29.93), 5,6)))))
  
  
  train$cHumidityRange <- ifelse(train$humidity >= 84.5, 1,
                                ifelse((train$humidity >= 74.5) & (train$humidity < 84.5), 2,
                                       ifelse((train$humidity >= 55.5) & (train$humidity < 74.5), 3,
                                              ifelse((train$humidity >= 39.5) & (train$humidity < 55.5), 4,
                                                     ifelse((train$humidity < 39.5), 5, 6)))))
  
  train$rHumidityRange <- ifelse(train$humidity >= 84.5, 1,
                                 ifelse((train$humidity >= 74.5) & (train$humidity < 84.5), 2,
                                        ifelse((train$humidity >= 66.5) & (train$humidity < 74.5), 3,
                                               ifelse((train$humidity >= 46.5) & (train$humidity < 66.5), 4,
                                                      ifelse((train$humidity < 46.5), 5, 6)))))
                                    
  
  train$rHourRange <- ifelse(train$hour %in% c(0,1,2,3,4,5), 1,
                            ifelse(train$hour %in% c(6,10,22,23), 2,
                                   ifelse(train$hour %in% c(9,11,12,13,14,15,20,21), 3,
                                          ifelse(train$hour %in% c(7,16,19), 4,
                                                 ifelse(train$hour %in% c(8,17,18),5,6)))))
  
  train$cHourRange <- ifelse(train$hour %in% c(1,2,3,4,5,6), 1,
                            ifelse(train$hour %in% c(0,7,23), 2,
                                   ifelse(train$hour %in% c(8,9,20,21,22), 3,
                                          ifelse(train$hour %in% c(10,11,18,19), 4,
                                                 ifelse(train$hour %in% c(12,13,14,15,16,17),5,6)))))
  
  
  train$hourRange <- ifelse(train$hour %in% c(0,1,2,3,4,5), 1,
                            ifelse(train$hour %in% c(6,22,23), 2,
                                   ifelse(train$hour %in% c(7,9,10,11,12,13,14,15,20,21), 3,
                                          ifelse(train$hour %in% c(8,16,19), 4,
                                                 ifelse(train$hour %in% c(17,18),5,6)))))
  
  train$tempRange <- as.factor(train$tempRange)
  train$cTempRange <- as.factor(train$cTempRange)
  train$rTempRange <- as.factor(train$rTempRange)
  
  train$cHumidityRange <- as.factor(train$cHumidityRange)
  train$rHumidityRange <- as.factor(train$rHumidityRange)
  
  train$hourRange <- as.factor(train$hourRange)
  train$cHourRange <- as.factor(train$cHourRange)
  train$rHourRange <- as.factor(train$rHourRange)
  
  return(train)
}

testDataProcessing <- function(test){
  
  test$year <- as.factor(year(ymd_hms(test$datetime)))
  test$month <- as.factor(month(ymd_hms(test$datetime)))
  test$hour <- as.factor(hour(ymd_hms(test$datetime)))
  test$weekdays <- as.factor(weekdays(ymd_hms(test$datetime)))
  test$dayType <- as.factor(ifelse((test$workingday==0)&(test$holiday==0),1,
                                   ifelse((test$workingday==0)&(test$holiday==1),2,
                                          ifelse((test$workingday==1)&(test$holiday==0),3,4))))
  test$diffTemp <- test$temp - test$atemp
  
  test$tempRange <- ifelse(test$temp < 12.71, 1,
                            ifelse((test$temp >=12.71) & (test$temp < 19.27), 2,
                                   ifelse((test$temp >=19.27) & (test$temp < 22.55), 3,
                                          ifelse((test$temp >=22.55) & (test$temp < 27.47), 4,
                                                 ifelse((test$temp >=27.47) & (test$temp < 29.93), 5,
                                                        ifelse((test$temp >=29.93), 6,7))))))
  
  test$cTempRange <- ifelse(test$temp < 12.71, 1,
                             ifelse((test$temp >=12.71) & (test$temp < 15.71), 2,
                                    ifelse((test$temp >=15.71) & (test$temp < 19.27), 3,
                                           ifelse((test$temp >=19.27) & (test$temp < 23.37), 4,
                                                  ifelse((test$temp >=23.37) & (test$temp < 29.93), 5,
                                                         ifelse((test$temp >=29.93), 6,7))))))
  
  
  test$rTempRange <- ifelse(test$temp < 12.71, 1,
                             ifelse((test$temp >=12.71) & (test$temp < 19.27), 2,
                                    ifelse((test$temp >=19.27) & (test$temp < 22.55), 3,
                                           ifelse((test$temp >=22.55) & (test$temp < 29.93), 4,
                                                  ifelse((test$temp >=29.93), 5,6)))))
  
  
  test$cHumidityRange <- ifelse(test$humidity >= 84.5, 1,
                                 ifelse((test$humidity >= 74.5) & (test$humidity < 84.5), 2,
                                        ifelse((test$humidity >= 55.5) & (test$humidity < 74.5), 3,
                                               ifelse((test$humidity >= 39.5) & (test$humidity < 55.5), 4,
                                                      ifelse((test$humidity < 39.5), 5, 6)))))
  
  test$rHumidityRange <- ifelse(test$humidity >= 84.5, 1,
                                 ifelse((test$humidity >= 74.5) & (test$humidity < 84.5), 2,
                                        ifelse((test$humidity >= 66.5) & (test$humidity < 74.5), 3,
                                               ifelse((test$humidity >= 46.5) & (test$humidity < 66.5), 4,
                                                      ifelse((test$humidity < 46.5), 5, 6)))))
  
  
  
  
  test$rHourRange <- ifelse(test$hour %in% c(0,1,2,3,4,5), 1,
                            ifelse(test$hour %in% c(6,10,22,23), 2,
                                   ifelse(test$hour %in% c(9,11,12,13,14,15,20,21), 3,
                                          ifelse(test$hour %in% c(7,16,19), 4,
                                                 ifelse(test$hour %in% c(8,17,18),5,6)))))
  
  test$cHourRange <- ifelse(test$hour %in% c(1,2,3,4,5,6), 1,
                             ifelse(test$hour %in% c(0,7,23), 2,
                                    ifelse(test$hour %in% c(8,9,20,21,22), 3,
                                           ifelse(test$hour %in% c(10,11,18,19), 4,
                                                  ifelse(test$hour %in% c(12,13,14,15,16,17),5,6)))))
  
  
  test$hourRange <- ifelse(test$hour %in% c(0,1,2,3,4,5), 1,
                           ifelse(test$hour %in% c(6,22,23), 2,
                                  ifelse(test$hour %in% c(7,9,10,11,12,13,14,15,20,21), 3,
                                         ifelse(test$hour %in% c(8,16,19), 4,
                                                ifelse(test$hour %in% c(17,18),5,6)))))
  
  test$tempRange <- as.factor(test$tempRange)
  test$cTempRange <- as.factor(test$cTempRange)
  test$rTempRange <- as.factor(test$rTempRange)
  
  test$cHumidityRange <- as.factor(test$cHumidityRange)
  test$rHumidityRange <- as.factor(test$rHumidityRange)
  
  test$hourRange <- as.factor(test$hourRange)
  test$cHourRange <- as.factor(test$cHourRange)
  test$rHourRange <- as.factor(test$rHourRange)
  
  return(test)
}

selectVariable <- function(data) {
  selected <- c(
    "season",
    "dayType",
    "weekdays",
    "weather",
    "temp",
    "atemp",
    "humidity",
    "windspeed",
    "diffTemp",
    "year",
    "hour"
    #"tempRange" remove
    #"rHumidityRange" remove
    #,"hourRange"
  )
  return(data[,selected])
}

selectVariable_c <- function(data) {
  selected <- c(
    #"season",
    "dayType",
    "weekdays",
    "weather",
    "temp",
    "atemp",
    "humidity",
    "windspeed",
    "diffTemp",
    #"year",
    "hour",
    "cTempRange",
    "cHumidityRange"
    ,"cHourRange"
  )
  return(data[,selected])
}

selectVariable_r <- function(data) {
  selected <- c(
    #"season",
    "dayType",
    "weekdays",
    "weather",
    "temp",
    "atemp",
    "humidity",
    "windspeed",
    "diffTemp",
    #"year",
    "hour",
    "rTempRange",
    "rHumidityRange"
    ,"rHourRange"
  )
  return(data[,selected])
}

