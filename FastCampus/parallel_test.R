library(topicmodels)
library(plyr)
library(snowfall)
library(foreach)
library(doSNOW)

# get data
data("AssociatedPress", package = "topicmodels")

# set number of topics to start with
k <- 20

# set model options
control_LDA_VEM <-
  list(estimate.alpha = TRUE, alpha = 50/k, estimate.beta = TRUE,
       verbose = 0, prefix = tempfile(), save = 0, keep = 0,
       seed = as.integer(100), nstart = 1, best = TRUE,
       var = list(iter.max = 10, tol = 10^-6),
       em = list(iter.max = 10, tol = 10^-4),
       initialize = "random")

# set sequence of topic numbers to iterate over
seq <- seq(2, 500, by = 100)




# set parallel processing options

# initiate cores
sfInit(parallel=TRUE, cpus=4, type="SOCK") # for snowfall
cl <- makeCluster(4, type = "SOCK") # for snow
registerDoSNOW(cl) # for snow

# send data and packages to multicores 
sfExport("AssociatedPress", "control_LDA_VEM") # for snowfall
sfLibrary(topicmodels) # for snowfall

# again for snow
clusterEvalQ(cl, library(topicmodels)) # for snow
clusterExport(cl, c("AssociatedPress", "control_LDA_VEM")) # for snow

# non-parallel methods
#base
BASE <- system.time(best.model.BASE <<- lapply(seq, function(d){LDA(AssociatedPress[1:20,], control = control_LDA_VEM, d)}))

# plyr non-parallel
PLYR_S <- system.time(best.model.PLYR_S <<- llply(seq, function(d){LDA(AssociatedPress[1:20,], control = control_LDA_VEM, d)}, .progress = "text"))

# parallel methods

# wrapper for some of these that don't handle the function well
wrapper <- function (d) topicmodels:::LDA(AssociatedPress[1:20,], control = control_LDA_VEM, d)

# using parLapply
PARLAP <- system.time(best.model.PARLAP <<- parLapply(cl, seq, wrapper))

# using dopar
DOPAR <- system.time(best.model.DOPAR <<- foreach(i = seq, .export = c("AssociatedPress", "control_LDA_VEM"), .packages = "topicmodels", .verbose = TRUE) %dopar% (LDA(AssociatedPress[1:20,], control = control_LDA_VEM, k=i)))

# using sfLapply                            
SFLAPP <- system.time(best.model.SFLAPP <<- sfLapply(seq, function(d){topicmodels:::LDA(AssociatedPress[1:20,], control = control_LDA_VEM, d)})) 

# using sfClusterApplyLB                            
SFCLU <- system.time(best.model.SFCLU <<- sfClusterApplyLB(seq, function(d){topicmodels:::LDA(AssociatedPress[1:20,], control = control_LDA_VEM, d)})) 

# using plyr in parallel (needs snow options)
PLYRP <- system.time(best.model.PLYRP <<- llply(seq, function(d){topicmodels:::LDA(AssociatedPress[1:20,], control = control_LDA_VEM, d)}, .parallel = TRUE))

# inspect results
rbind(BASE, PLYR_S, PARLAP, DOPAR, SFLAPP, SFCLU, PLYRP)


#----------------------------------------------------------------------------------

# set number of topics to start with
k <- 20

# set model options
control_LDA_VEM <-
  list(estimate.alpha = TRUE, alpha = 50/k, estimate.beta = TRUE,
       verbose = 0, prefix = tempfile(), save = 0, keep = 0,
       seed = as.integer(100), nstart = 1, best = TRUE,
       var = list(iter.max = 10, tol = 10^-6),
       em = list(iter.max = 10, tol = 10^-4),
       initialize = "random")

cl <- makeCluster(4, type = "SOCK") # for snow
registerDoSNOW(cl) # for snow

clusterEvalQ(cl, library(topicmodels)) # for snow
clusterExport(cl, c("new_dtm", "control_LDA_VEM")) # for snow

# set sequence of topic numbers to iterate over
seq <- seq(10, 30, by = 1)

# wrapper for some of these that don't handle the function well
wrapper <- function (d) topicmodels:::LDA(new_dtm, control = control_LDA_VEM, d)

# using parLapply
PARLAP <- system.time(best.model.PARLAP <<- parLapply(cl, seq, wrapper))

for(i in 1:21){
  print(perplexity(best.model.PARLAP[[i]]))
}

