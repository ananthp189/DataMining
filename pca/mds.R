euromat <- eurodist
euro.cmds <- cmdscale(euromat, k=2)

plot(euro.cmds[,1], euro.cmds[,2], xlab="Coordinate 1",
     ylab="Coordinate 2", main="classical MDS", type="n",asp=1)
text(euro.cmds[,1], euro.cmds[,2],labels = labels(euromat),cex=.7)

library(smacof)
crimes
crime.dist <- sim2diss(crimes, method="corr")
set.seed(1)
crime.mds <- mds(crime.dist, ndim=2, type="interval")
# crime.mds$conf #matrix of fitted configurations
plot(crime.mds,xlab="Axis 1", ylab="Axis 2")
crime.mds1 <- mds(crime.dist, ndim=1, type="interval")
plot(crime.mds1$conf,rep(0,7),xlab="axis 1",ylab="",yaxt="n")
text(crime.mds1$conf,0.5+rep(0,7),labels=names(crimes),cex=.7)
plot(crime.mds, plot.type="Shepard", main="Shepard diag. 2D")
plot(crime.mds1, plot.type="Shepard", main="Shepard diag. 1D")

cor(as.matrix(crime.mds1$confdist)[lower.tri(crime.mds1$confdist)],
    as.matrix(crimes)[lower.tri(crimes)])
cor(as.matrix(crime.mds$confdist)[lower.tri(crime.mds1$confdist)],
    as.matrix(crimes)[lower.tri(crimes)])

library(MASS);library(smacof)
crime.dist <- sim2diss(crimes, method="corr")
set.seed(1)
crime.sm<-sammon(crime.dist,k=2)
plot(crime.sm$points,type="n",asp=1)
text(crime.sm$points,labels=names(crimes))
set.seed(1)
crime.mds <- mds(crime.dist, ndim=2, type="interval")
par(mfrow=c(1,2))
plot(as.dist(crime.dist),crime.mds$confdist,pch=16,xlab="Dissimilarities",
     ylab="Configuration Distances", main="Shepard diag.(metric MDS)")
crime.mds.fit <- lm(as.vector(crime.mds$confdist)~as.vector(as.dist(crime.dist)))
abline(crime.mds.fit,col="red",lty=2)
plot(as.dist(crime.dist),dist(crime.sm$points),pch=16,xlab="Dissimilarities",
     ylab="Configuration Distances", main="Shepard diag.(Sammon)")
crime.sm.fit <- lm(as.vector(dist(crime.sm$points))~as.vector(as.dist(crime.dist)))
abline(crime.sm.fit,col="red",lty=2)
par(mfrow=c(1,1))


wish
diss <- sim2diss(wish, method = 7) #sim. to dissim. by subtracting from 7
set.seed(1)
res <- mds(diss, type = "ordinal")
plot(res,asp=1)
