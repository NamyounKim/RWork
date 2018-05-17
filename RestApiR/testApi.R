library(jsonlite)

#* @get /getMean
normalMean <- function(samples=10){
  data <- rnorm(samples)
  toJSON(data)
  #mean(data)
}

#* @get /getData
showData <- function(i=10){
  data = matrix(1:i, nrow = 100, ncol = 3)
  data = as.data.frame(data)
  toJSON(data)
  #mean(data)
}

#* @post /sum
addTwo <- function(a, b){
  as.numeric(a) + as.numeric(b)
}