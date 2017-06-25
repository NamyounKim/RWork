ids <- c(rep("X", 5), rep("Y", 5), rep("Z", 5))
seq <- rep(1:5,3)
val <- sample(LETTERS, 15, replace=T)
df <- data.frame(ids, seq, val)

require(arulesSequences)

x <- read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), info = c("sequenceID","eventID","SIZE"))
x
x_df=as(x,"data.frame")
s1 <- cspade(x, parameter = list(support = 0.4), control = list(verbose = TRUE))
summary(s1)

s2 <- apriori(x, parameter = list(support = 0.4), control = list(verbose = TRUE))

