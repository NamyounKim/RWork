library(FactoMineR)
data(wine)

res = MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)), ncp=5, name.group=c("origin","odor","visual","odor.after.shaking", "taste","overall"), num.group.sup=c(1,6))
plot(res,choix="ind",partial="all")

data("iris")
iris

iris2 <- iris[,1:4]
iris2
fit <- princomp(iris2, cor=TRUE)
fit2 <- principal(iris2, nfactors=4, rotate="varimax")


fit3 <- factanal(iris2, 1, rotation="varimax")
print(fit3, digits=2, cutoff=.3, sort=TRUE)
load <- fit3$loadings[,1] 
plot(load,type="n")
text(load,labels=names(iris2),cex=.7)
