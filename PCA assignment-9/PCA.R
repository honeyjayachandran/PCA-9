#install.packages("gdata")
#install.packages("xlsx") 
library(gdata)
library(xlsx)
PCA<-read.csv(file = file.choose())

pca<-princomp(PCA, cor = TRUE, scores = TRUE,covmat = NULL)

summary(pca)

df <- as.data.frame(pca$scores)

df1 <- df[,1:7]



plot(pca$scores[,1:7],col="Blue",pch=18,cex = 0.3, lwd = 3)
text(pca$scores[,1:7], labels=c(1:25), cex= 1)

library(NbClust)



#### kmeans for pca scores

library(factoextra)

fviz_nbclust(df1,kmeans,"wss")


fviz_nbclust(df1,kmeans,"silhouette")

pca_kmeans <- kmeans(x = df1,centers = 3,nstart = 25)

df1$cluster <- pca_kmeans$cluster

#### hclust for pca scores 

library(cluster)

pca_hclust <- agnes(df,method = "ward")

pca_hclust$ac

pltree(pca_hclust,hang = -1)

attributes(pca_hclust)

#### kmeans for orginal data

library(factoextra)

fviz_nbclust(PCA,FUNcluster = kmeans,method = "wss")

PCA_KMEANS <- kmeans(PCA,centers = 3,nstart = 25)

PCA$cluster <- PCA_KMEANS$cluster

#### hclust for original data

library(cluster)

PCA_hclust <- agnes(PCA,method = "ward")

pltree(PCA_hclust,hang=-1)

cutree(PCA_hclust,k = 3)
