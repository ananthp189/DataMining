#classificatino evaluations
# If we want the split to be 50%, 25% and 25% we first have to
# get the indices of observations belonging to each subset
n <- nrow(iris)
ind1 <- sample(c(1:n),round(n/2))
ind2 <- sample(c(1:n)[-ind1],round(n/4))
ind3 <- setdiff(c(1:n),c(ind1,ind2))
train.data <- iris[ind1, ]
valid.data <- iris[ind2, ]
test.data <- iris[ind3, ]
dim(train.data)
#Set up a vector of 5 numbers to store the classification rates for each set
#when it is used as a validation set
#Assume our data is in a matrix called data
#Our class variable is called y
#The generic method for fitting some model is called function.fit
#with the corresponding prediction function, function.pred
corr.class.rate<-numeric(5)
for (i in 1:5) {
  #For each run, we use one set for test/validation
  test.data <- data[ind[[i]],]
  test.label <- y[ind[[i]]]
  #The remaining sets are our training data
  train.ind <- setdiff(c(1:n),ind[[i]])
  train.data <- data[train.ind,]
  train.label <- y[train.ind]
  #Fit the model on the training data
  model.fit <- function.fit(train.data, train.label)
  #Predict using the test data
  pred.class <- function.pred(model.fit, test.data)
  #Calculate the test data correct classification rate
  9
  corr.class.rate[i] <- sum(test.label==pred.class)/length(test.label)
}
#average the 5 correct classification rates to find the overall rate
cv.corr.class.rate <- mean(corr.class.rate)

#KNN example:
# Create the data and the classes
library(class)
n <- 100; n.val <- 10000
set.seed(1)
x <- round(runif(n, 1, n))
set.seed(2)
y <- round(runif(n, 1, n))
train.df <- data.frame(x, y)
set.seed(3)
x.val <- round(runif(n.val, 1, n))
set.seed(4)
y.val <- round(runif(n.val, 1, n))
val.df <- data.frame(x.val, y.val)
classes <- ifelse(x^2 + y^2 > 60^2, "blue", "orange")
classes.val <- ifelse(x.val^2 + y.val^2 > 60^2, "blue", "orange")
# Run kNN for different values of k from 1 to 25 and record the validation error rate
class.rate<-numeric(25)
for(k in 1:25) {
  pred.class <- knn(train.df, val.df, classes, k=k)
  class.rate[k] <- sum(pred.class==classes.val)/length(pred.class)
}
plot(c(1:25), class.rate, type="b",
     main="Correct classification rates on the validation data for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)

grid <- expand.grid(x=1:100, y=1:100)
k.opt <- which.max(class.rate)
classes.grid <- knn(train.df, grid, classes, k=k.opt, prob=TRUE) #note last argument
prob.grid <- attr(classes.grid, "prob")
prob.grid <- ifelse(classes.grid == "blue", prob.grid, 1 - prob.grid)
contour(x=1:100, y=1:100, z=matrix(prob.grid, nrow=100), levels=0.5,
        col="grey", drawlabels=FALSE, lwd=2) #plot the boundary
points(train.df, col=classes,pch=20) # add points from train data set
title(main=paste("k =",k.opt),cex.main=0.9,font.main=4)
