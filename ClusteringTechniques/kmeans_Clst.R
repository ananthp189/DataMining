#--------------------------------------------------------------------------------- 
# STATS5099 Data Mining Tutorial 9
#---------------------------------------------------------------------------------
# Applied Question 1
#---------------------------------------------------------------------------------

Iris <- iris[,-c(5)]
Iris <- scale(Iris)

################################
# Select the number of cluster #
################################
set.seed(1)

# Hierarchical agglomerative clustering
library(factoextra)
ggplot_fviz <-fviz_nbclust(USArrests,FUN=hcut,method="silhouette")
ggplot_fviz
#If the average silhouette width is used as the evaluation criterion,
#then we will select two clusters.
ggplot_fviz <-fviz_nbclust(USArrests,FUN=hcut,method="gap_stat")
ggplot_fviz
#If the Gap statistic is used as the evaluation criterion,
#then we will select five clusters.

# K-means clustering
ggplot_fviz <-fviz_nbclust(USArrests,FUN=kmeans,method="silhouette")
ggplot_fviz #2 clusters are suggested
ggplot_fviz <-fviz_nbclust(USArrests,FUN=kmeans,method="gap_stat")
ggplot_fviz #3 clusters are suggested

# K-medoids clustering
ggplot_fviz <-fviz_nbclust(USArrests,FUN=cluster::pam,method="silhouette")
ggplot_fviz #2 clusters are suggested
ggplot_fviz <-fviz_nbclust(USArrests,FUN=cluster::pam,method="gap_stat")
ggplot_fviz #6 clusters are suggested

####################################
# Evaluate clustering performances #
####################################
set.seed(1)

#Suppose we select 2 clusters for all three methods and
#compare the clustering results using the silhouette width
Iris.HAC <- hclust(dist(Iris))
Iris.HAC.clus <- cutree(Iris.HAC,k=2)

Iris.km <- kmeans(Iris,centers=2,nstart=50)
Iris.km.clus <- Iris.km$cluster

library(cluster)
Iris.PAM <- pam(Iris,k=2,nstart=50)
Iris.PAM.clus <- Iris.PAM$clustering

Iris.HAC.si <- silhouette(Iris.HAC.clus, dist(Iris))
Iris.km.si  <- silhouette(Iris.km.clus, dist(Iris))
Iris.PAM.si <- silhouette(Iris.PAM.clus, dist(Iris))
windows()
par(mfrow=c(1,3))
plot(Iris.HAC.si)
plot(Iris.km.si)
plot(Iris.PAM.si)
#K-means and K-medoids generate same clustering results;
#their average silhouette width is larger than that of HAC.

################################################
# Compare clustering results with class labels #
################################################

#For more details, see supplementary material on Week 9 P17
library(fpc)
HAC_baseline <- cluster.stats(
  dist(Iris),as.numeric(iris$Species),Iris.HAC.clus)
HAC_baseline
km_baseline <- cluster.stats(
  dist(Iris),as.numeric(iris$Species),Iris.km.clus)
km_baseline
PAM_baseline <- cluster.stats(
  dist(Iris),as.numeric(iris$Species),Iris.PAM.clus)
PAM_baseline
print(c(HAC_baseline$corrected.rand,
        km_baseline$corrected.rand,
        PAM_baseline$corrected.rand))
#For example, we could compare the cluster allocations with
#class labels based on adjusted Rand index. Larger values
#suggest higher consistency between the pair of points 
#from clustering and the pair from class labels.