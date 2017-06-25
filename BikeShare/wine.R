
wine_red <- read.csv(file = "./1_wine/winequality-red.csv" , sep=";")
wine_white <- read.csv(file="./1_wine/winequality-white.csv", sep=";")

head(wine_red)
wine_red$quality <- factor(wine_red$quality)


fit <- princomp(wine_red[,-12], cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

library(psych)
fit <- principal(wine_red[,-12], nfactors=5, rotate="varimax")
fit # print results


#Exploratory Factor Analysis
fit2 <- factanal(wine_red[,-12], 3, rotation="varimax")
print(fit2, digits=2, cutoff=.3, sort=TRUE)
print(fit2$loadings, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit2$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(wine_red[,-12]),cex=.7) # add variable names



#Using fa() from package psych with rotation
corMat <- cor(wine_red[,-12])
faPC  <- fa(r=corMat, nfactors=4, rotate="varimax")
factor.plot(faPC, cut=0.5)
fa.diagram(faPC)
#Determine number of factors
fa.parallel(wine_red[,-12])
vss(wine_red[,-12], rotate="varimax") 
