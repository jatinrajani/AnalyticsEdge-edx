
load("~/Mitedx/LinearRegression/ClimateChange/.RData")
load("~/AnalyticsEdge-edx/Clustering/Image Segmentation/flower.csv")
setwd("~/AnalyticsEdge-edx/Clustering/Image Segmentation/brain")
healthy=read.csv("healthy",header=FALSE)
healthy=read.csv("healthy.csv",header=FALSE)
healthymat=as.matrix(healthy)
str(healthymat)
image(healthymat,axes=FALSE,col=grey(seq(0,1,length=256)))
healthyvector=as.vector(healthymat)
distance=dist(healthyvector,method="euclidean")
str(healthyvector)
n=365636
n*(n-1)/2
k=5
set.seed(1)
KMC=kmeans(healthyvector,centers=k,iter.max=1000)
str(KMC)
healthycluster=KMC$cluster
KMC$size[2]
dim(healthycluster)=dim(nrow(healthymat),ncol(healthymat))
dim(healthycluster)=c(nrow(healthymat),ncol(healthymat))
image(healthycluster,axes=FALSE,col=rainbow(5))
tumor=read.csv("tumor.csv",header=FALSE)
tumormat=as.matrix(tumor)
tumorvector=as.vector(tumormat)
install.packages("flexclust")
library(flexclust)
KMC.kccaa=as.kcca(KMC,healthyvector)
tumorclusters=predict(KMC.kcca,newdata=tumorvector)
tumorclusters=predict(KMC.kccaa,newdata=tumorvector)
dim(tumorclusters)=c(nrow(tumormat),ncol(tumormat))
image(tumorclusters,axes=FALSE,col=rainbow(k))
