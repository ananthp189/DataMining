#K means Clustering and K-medoids
#Kmeans is most used
#K->number of clusters needed
set.seed(2018);library(MASS)
x<-mvrnorm(200,c(0,0),diag(1,2))
y<-mvrnorm(150,c(4,-5),diag(3,2))
data<-rbind(x,y)
plot(data,type="n",xlab="x",ylab="y")
true.label<-c(rep(1,200),rep(2,150))
mtext("Simulated data set",line=2)
mtext("(from two normal distributions)",line=1)
points(data[true.label==1,],col="red",pch=20)
points(data[true.label==2,],col="blue",pch=20)

#kmeans
set.seed(2020)
k<-3 #number of clusters
n<-nrow(data) # number of rows
start.ind<-sample(1:n,k) # take a sample of size/length k out of the n rows

start.centres<-data[start.ind,] # use the indices from the previous sample to select
# the data points that will represent the centers
# of each cluster
plot(data,pch=20,xlab="x",ylab="y")
points(start.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=2)
#understanding purposes only, simple kmeans command available in r
#Function to find the latest assignment of points to clusters
#based on the current cluster centres
find.clust.assign<-function(data,clust.centres){
  #Assume rows of data are observations, cols are variables.
  #Assume rows of clust.centres are each cluster, cols are variables.
  n<-nrow(data)
  k<-nrow(clust.centres)
  temp<-rbind(data,clust.centres)
  d<-as.matrix(dist(temp))
  #d will be a distance matrix between all pairs of points and cluster centers
  #The last k rows will be the distances between the points and the cluster centres
  #and the distance between the cluster centres themselves
  centre.dist<-d[(n+1):(n+k),-c((n+1):(n+k))]
  #Find out which of the k cluster centres is closest to each point
  assign.clust<-apply(centre.dist,2,which.min)
  return(assign.clust)
}

#Function to find the latest cluster centres
#based on the current assignment of points to clusters
find.clust.centre<-function(data,assign.clust){
  #Assume rows of data are obs., cols are vars.
  #Assume assign.clust is a vector of length equal to the no. of obs.
  #Assume that the cluster numbers in assign.clust run from 1 up to the number of clusters
  k<-max(assign.clust)
  d<-ncol(data)
  clust.centres<-matrix(NA,k,d)
  for(j in 1:k) {
    #For each cluster average the points assigned to that cluster
    clust.centres[j,]<-apply(data[assign.clust==j,],2,mean)
  }
  return(clust.centres)
}


#Function to find the latest cluster centres
#based on the current assignment of points to clusters
find.clust.centre<-function(data,assign.clust){
  #Assume rows of data are obs., cols are vars.
  #Assume assign.clust is a vector of length equal to the no. of obs.
  #Assume that the cluster numbers in assign.clust run from 1 up to the number of clusters
  k<-max(assign.clust)
  d<-ncol(data)
  clust.centres<-matrix(NA,k,d)
  for(j in 1:k) {
    #For each cluster average the points assigned to that cluster
    clust.centres[j,]<-apply(data[assign.clust==j,],2,mean)
  }
  return(clust.centres)
}
first.assign<-find.clust.assign(data,start.centres)
plot(data,type="n",xlab="x",ylab="y")
points(data[first.assign==1,],col=adjustcolor("red",alpha=0.3),pch=20)
points(data[first.assign==2,],col=adjustcolor("blue",alpha=0.3),pch=20)
points(data[first.assign==3,],col=adjustcolor("green",alpha=0.3),pch=20)
points(start.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=3)


#Let’s start with computing the new centroids of each cluster.
plot(data,type="n",xlab="x",ylab="y")
points(data[first.assign==1,],col=adjustcolor("red",alpha=0.3),pch=20)
points(data[first.assign==2,],col=adjustcolor("blue",alpha=0.3),pch=20)
points(data[first.assign==3,],col=adjustcolor("green",alpha=0.3),pch=20)
first.centres<-find.clust.centre(data,first.assign)
points(start.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=3)
points(first.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=3)
arrows(start.centres[,1],start.centres[,2],first.centres[,1],first.centres[,2],
       code=2,length=0.08,lwd=1)
#NWe now allocate each data point to the cluster whose centroid is closest.
second.assign<-find.clust.assign(data,first.centres)
plot(data,type="n",xlab="x",ylab="y")
points(data[second.assign==1,],col=adjustcolor("red",alpha=0.3),pch=20)
points(data[second.assign==2,],col=adjustcolor("blue",alpha=0.3),pch=20)
points(data[second.assign==3,],col=adjustcolor("green",alpha=0.3),pch=20)
points(first.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=3)
#visualize changes
plot(data,type="n",main="Computing new centroids",xlab="x",ylab="y",cex.main=0.8)
points(data[second.assign==1,],col=adjustcolor("red",alpha=0.3),pch=20)
points(data[second.assign==2,],col=adjustcolor("blue",alpha=0.3),pch=20)
points(data[second.assign==3,],col=adjustcolor("green",alpha=0.3),pch=20)
second.centres<-find.clust.centre(data,second.assign)
points(first.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=3)
points(second.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=3)
arrows(first.centres[,1],first.centres[,2],second.centres[,1],second.centres[,2],
       code=2,length=0.08,lwd=1)


#Now, we can re-allocate each data point again to the nearest cluster.
third.assign<-find.clust.assign(data,second.centres)
plot(data,type="n",main="Assign data points to clusters",xlab="x",ylab="y",cex.main=0.8)
points(data[third.assign==1,],col=adjustcolor("red",alpha=0.3),pch=20)
points(data[third.assign==2,],col=adjustcolor("blue",alpha=0.3),pch=20)
points(data[third.assign==3,],col=adjustcolor("green",alpha=0.3),pch=20)
points(second.centres,pch=4,col=c("red","blue","green"),cex=1.5,lwd=3)
#If we want to continue this procedure (even though we don't have to)
#third.centres<-find.clust.centre(data,third.assign)

#Kmeans in R
set.seed(1984)
mydata <- data
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) #total sum of squares
for (i in 2:15) wss[i] <- kmeans(mydata, centers=i, nstart=100)$tot.withinss
plot(1:15, wss, type="b", pch=20, cex=1.5, cex.main=0.9,
     xlab="Number of clusters", ylab="Within-cluster sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method")

set.seed(1984)
km2 <- kmeans(data, 2, nstart=100)
km2

plot(data, col=(km2$cluster +1), main="K-means result with 2 clusters",
     pch=20, cex=2, xlab="x",ylab="y", cex.main=0.8)

#silhoutte plot for interpretting
library(cluster)
cluster.labels.2 <- km2$cluster
si2 <- silhouette(cluster.labels.2, dist(data))
summary(si2)
plot(si2)

set.seed(1984)
km3 <- kmeans(data, 3, nstart=100)
km3
cluster.labels.3 <- km3$cluster
si3 <- silhouette(cluster.labels.3, dist(data))
summary(si3)
plot(si3)



#Kmediod - partitioning around medoids

library(cluster)
# To fit k-medoids for 2 clusters:
res.pam <- pam(data, 2)
plot(res.pam)

