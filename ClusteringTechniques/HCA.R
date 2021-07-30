#HCA for EDA, PatternDetection, Pre Processing, Dimensionality reduction,etc
data("USArrests")
str(USArrests)
head(USArrests)

summary_stats <- data.frame(
  Min = apply(USArrests, 2, min), # minimum
  Med = apply(USArrests, 2, median), # median
  Mean = apply(USArrests, 2, mean), # mean
  SD = apply(USArrests, 2, sd), # standard deviation
  Max = apply(USArrests, 2, max) # maximum
)
summary_stats <- round(summary_stats, 1);summary_stats

boxplot(USArrests)

#scale 
USArrests <- scale(USArrests)
head(USArrests)

distance_arrests <- as.matrix(dist(USArrests))
distance_arrests[1:3,1:3]

#algorithmic clustering _> heirarchial and kmeans

#heierarchial clusterig -> agglomerative and divisive
usarrests.clus <- hclust(dist(USArrests))
# This gives us an object of class hclust that is comprised of various components.
# plot(usarrests.clus,xlab="complete linkage",sub="")
# We can definitely use the plot command but in this case we will use a specific
# function for handling tree-like structures.
# Specifically, we will create a dendrogram plot with horizontal labels
# but also having the leaves hang according to their height.
plot(as.dendrogram(usarrests.clus,hang = 0),ylab="complete linkage",
     xlab="Euclidean distance",horiz=TRUE)


library(dendextend)
usarrests.clus <- hclust(dist(USArrests))
dend_usarrests <- as.dendrogram(usarrests.clus,hang=0)
dendrogram_cut_3clusters <- color_branches(dend_usarrests,k=3) #3 clusters
plot(dendrogram_cut_3clusters,ylab="complete linkage",
     xlab="Euclidean distance",horiz=TRUE)
allocations <- cutree(dend_usarrests,k=3)
head(allocations)
# get cluster means
aggregate(USArrests,by=list(allocations),FUN=mean)
# append cluster assignment
df <- data.frame(USArrests,allocations)
head(df)


usarrests.clus <- hclust(dist(USArrests))
dend_usarrests <- as.dendrogram(usarrests.clus,hang = 0.0)
dendrogram_cut_4height <- color_branches(dend_usarrests,h=4) #height of 4
plot(dendrogram_cut_4height,ylab="complete linkage",
     xlab="Euclidean distance",horiz=TRUE)
abline(v=4,lty=2,lwd=2)

usarrests.labels <- cutree(usarrests.clus,h=4)
pairs(USArrests,col=usarrests.labels,lower.panel=NULL)

library(cluster)
usarrests.clus <- hclust(dist(USArrests))
dend_usarrests <- as.dendrogram(usarrests.clus,hang = 0.0)
allocations_4height <- cutree(dend_usarrests,h=4)
allocations_2clusters <- cutree(dend_usarrests,k=2)
par(mfrow=c(1,2))
plot(silhouette(allocations_4height,dist(USArrests)),
     col = c("black", "red", "green", "blue"),
     main="Silhouette plot (dendrogram cut at height 4)")
plot(silhouette(allocations_2clusters,dist(USArrests)),
     col = c("lightgreen", "lightblue"),
     main="Silhouette plot (dendrogram with 2 clusters)")
#optimal number of clusters
library(factoextra)
ggplot_fviz <-fviz_nbclust(USArrests,FUN=hcut,method = "silhouette")
ggplot_fviz