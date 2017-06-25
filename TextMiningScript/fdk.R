library(doMC)

data.dir <- '/home/ruser/TextPrism/Kaggle/fkd/'

train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')

d.train <- read.csv(train.file, stringsAsFactors=F)
im.train <- d.train$Image
d.train$Image <- NULL

registerDoMC()
#convert these strings to integers
as.integer(unlist(strsplit(im.train[1], " ")))

im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL


#========================================================
im2 <- matrix(data=rev(im.train[1,]), nrow=9216, ncol=9216)
image(1:9216, 1:9216, im2, col=gray((0:255)/255))
