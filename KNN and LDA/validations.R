#-----------------------
# Task 4
#-----------------------

#create training/validation/test set
set.seed(1)
data(iris)
n <- nrow(iris)
ind1 <- sample(c(1:n),round(n/2))
ind2 <- sample(c(1:n)[-ind1],round(n/4))
ind3 <- setdiff(c(1:n),c(ind1,ind2))
train.data <- iris[ind1,]
valid.data <- iris[ind2,]
test.data  <- iris[ind3,]

#choose k based on validation set
library(class)
K <- 25
corr.class.rate<-numeric(K)
for(k in 1:K){
  pred.class<-knn(train.data[,-5], valid.data[,-5], train.data[,5], k=k)
  corr.class.rate[k]<-mean(pred.class==valid.data$Species)
}

plot(c(1:K),corr.class.rate,type="b",
     main="Correct classification rates on the validation data for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)

k.opt <- which.max(corr.class.rate);k.opt

#choose k using leave-out-out cross validation
K <- 25
LOOCV.rate<-numeric(K)
for(k in 1:K){
  pred.class<-knn.cv(train.data[,-5], train.data[,5], k=k)
  LOOCV.rate[k]<-mean(pred.class==train.data$Species)
}

plot(c(1:K),LOOCV.rate,type="b",
     main="LOOCV correct classification rates for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)

which.max(LOOCV.rate)

#Question: how to select k using 5-fold cross validation?

pred<-knn(train.data[,-5], test.data[,-5], train.data[,5], k=k.opt)
acc <- mean(pred==test.data$Species);acc

#-----------------------
# Task 6
#-----------------------

#numerical summaries
for (i in 1:4){
  cat(colnames(train.data)[i],sep="\n")
  print(aggregate(train.data[,i],by=list(train.data$Species),var))
}
# cov(train.data[train.data$Species=="setosa",1:4]) #can be used to check covariance

#graphical summaries
library(GGally)
ggpairs(iris, columns=1:4, ggplot2::aes(colour=Species,alpha=0.2))

#implement LDA
library(MASS)
iris.lda <- lda(Species~., data=train.data) #training and test data are same as Task 4
iris.lda


#visualise how LD1 separates the three classes by using ldahist function
iris.pred.tr <- predict(iris.lda)
windows()
ldahist(data = iris.pred.tr$x[,1], g=train.data$Species)
#or by using ggplot
dataset <- data.frame(Type=train.data$Species, lda=iris.pred.tr$x)
ggplot(dataset, aes(x=lda.LD1)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)

#create a classification rule based on LD1
pred.LD1 <-rep("versicolor",nrow(train.data))
pred.LD1[iris.pred.tr$x[,1] < -3.5] <- "virginica"
pred.LD1[iris.pred.tr$x[,1] >  3] <- "setosa"
sum(pred.LD1!=train.data$Species)

#visualise how LD1 and LD2 together separate the three classes
plot(iris.pred.tr$x[,1],iris.pred.tr$x[,2], col=train.data$Species,
     pch=as.numeric(train.data$Species), xlab="LD1", ylab="LD2")
ggplot(dataset, aes(x=lda.LD1, y=lda.LD2)) + 
  geom_point(aes(group=Type, colour=Type, shape=Type))

#visualise the decision boundary
library(klaR)
partimat(Type ~ ., data = dataset, method = "lda")
# partimat(Species ~ ., data = iris, method = "lda")
# partimat(Species ~ ., data = iris, method = "qda")

#predict species on test set
iris.pred.te <- predict(iris.lda)
acc <- mean(train.data$Species == iris.pred.te$class);1-acc



#-----------------------
# Remark: advanced visualisation methods, e.g. ggpairs, partimat,
# will not be examined. 
#-----------------------
# For more examples, incl. admission data in the lecture video, see:
# https://rpubs.com/gabrielmartos/discriminantR
#-----------------------