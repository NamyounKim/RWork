library(cluster)
library(RGtk)

target = rawData %>% select(발색,커버,밀착력,cleansing,흡수력,보습,주름,영양,휴대,vfm) %>% filter(rowSums(.)>0)

result = kmeans(target, centers = 9, iter.max = 10000)
result
par(mfrow=c(1,1))
clusplot(target, clus = result$cluster, color = T, shade = T, labels = 2)

rawData$cluster = result$cluster

par(mfrow=c(3,3))
barplot(colMeans(rawData %>% filter(cluster==1) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==1)))
barplot(colMeans(rawData %>% filter(cluster==2) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==2)))
barplot(colMeans(rawData %>% filter(cluster==3) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==3)))
barplot(colMeans(rawData %>% filter(cluster==4) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==4)))
barplot(colMeans(rawData %>% filter(cluster==5) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==5)))
barplot(colMeans(rawData %>% filter(cluster==6) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==6)))
barplot(colMeans(rawData %>% filter(cluster==7) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==7)))
barplot(colMeans(rawData %>% filter(cluster==8) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==8)))
barplot(colMeans(rawData %>% filter(cluster==9) %>% select(9:18)), ylim = c(0,0.5), main = nrow(rawData %>% filter(cluster==9)))




d = dist(target[,2:11], method = "euclidean")
fit = hclust(d, method = "ward")
plot(fit)
groups = cutree(fit, k=5)
rect.hclust(fit, k = 5, border = "red")

fit_pca = princomp(target, cor = T)
loadings(fit_pca)
biplot(fit_pca, choices = c(1,2,3))
