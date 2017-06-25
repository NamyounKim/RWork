#data.dir   <- '/Users/kimnamyoun/Documents/RWork/Kaggle/Facial Keypoints Detection/'
data.dir <- 'D:/PrivateData/Kaggle/Facial Keypoints Detection/'
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')

d.train <- read.csv(train.file, stringsAsFactors=F)

im.train      <- d.train$Image
d.train$Image <- NULL

library(foreach)

im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL

im <- matrix(data=rev(im.train[1,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

d <- data.frame(x=1:10, y=rnorm(10))
s <- foreach(d=iter(d, by='row'), .combine=rbind) %dopar% d
identical(s, d)