library(FactoMineR)

subExpend_p = filter(subExpend, !(category2 %in% c('보장성보험')))
subExpend_p = subset(subExpend_p, yearMonth >='2015-01')
subExpend_p = filter(subExpend_p, yearMonth <= '2015-12')
head(subExpend_p)

castExpend = cast(subExpend_p, yearMonth~category2, sum, value='totalExpend')
castExpend = select(castExpend, -yearMonth)
fit <- princomp(castExpend, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type="lines")
fit$scores # the principal components
biplot(fit)

result <- PCA(castExpend)


fit <- factanal(castExpend, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(castExpend),cex=.7) # add variable names
