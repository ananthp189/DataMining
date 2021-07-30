set.seed(147)
library(MASS)
library(e1071)
data(crabs)
dim(crabs)
#Split the data 50% training, 25% validation and 25% test
n<-nrow(crabs)
intrain<-sample(c(1:n),round(n/2))
invalid<-sample((c(1:n)[-intrain]),round(n/4))
train.data<-crabs[intrain,-c(2:3)]
valid.data<-crabs[invalid,-c(2:3)]
test.data<-crabs[-c(intrain,invalid),-c(2:3)]
#Fit the classification SVM for different values of C
#and calculate the validation prediction error
pred.error<-function(pred,truth){
  mean(pred!=truth)
}
C.val <- c(0.1,0.5,1,2,5,10)
C.error <- numeric(length(C.val))
for (i in 1:length(C.val)) {
  model <- svm(sp~.,data=train.data,type="C-classification",kernel="linear",
               cost=C.val[i]) #kernel will be explained in the next section
  9
  pred.model <- predict(model, valid.data)
  C.error[i] <- pred.error(pred.model, valid.data$sp)
}
C.sel <- C.val[min(which.min(C.error))]
C.sel

plot(C.val,C.error,type="b")
abline(v=C.sel,lty=2)
#We can also use the tune.svm command to do something similar
#using cross validation on the training data.
tune.svm(sp~.,data=train.data,type="C-classification",kernel="linear",
         cost=C.val)

final.svm<-svm(sp~.,data=train.data,kernel="linear",cost=C.sel,type="C-classification")
summary(final.svm)
pred.test<-predict(final.svm,test.data)
pred.error(pred.test,test.data$sp)
table(test.data$sp,pred.test)
final.svm$index
#low cost, high sv
extra.svm<-svm(sp~.,data=train.data,kernel="linear",cost=C.sel/2,
               type="C-classification")
extra.svm

#large cost low sv
extra.svm2<-svm(sp~.,data=train.data,kernel="linear",cost=C.sel*1000,
                type="C-classification")
extra.svm2


# these are hard bound svms, can be kernalized to improve :
# linear kernel: 𝑘 (𝒙𝑖
#                    , 𝒙 𝑗) = 𝒙
# ⊺
# 𝑖
# 𝒙 𝑗
# . This gives the SVM we saw earlier. The corresponding transformation is simply
# the identity function, i.e. 𝜙(𝒙) = 𝒙.
# • polynomial kernel of degree 𝑑: 𝑘 (𝒙𝑖
#                                       , 𝒙 𝑗) = (1 + 𝒙
#                                                   ⊺
#                                                   𝑖
#                                                   𝒙 𝑗)
# 𝑑
# , where 𝑑 is a positive integer. When 𝑑 > 1, we are
# essentially fitting a hyperplane in a higher-dimensional space involving polynomials of degree 𝑑, which leads to
# a much more flexible decision boundary.
# • radial basis function (RBF) kernel (also known as Gaussian kernel): 𝑘 (𝒙𝑖
#                                                                           , 𝒙 𝑗) = exp(−𝛾||𝒙𝑖 − 𝒙 𝑗
#                                                                                             ||2
#                                                                           ), where 𝛾 is a
# positive constant. The RBF kernel has very local behaviour. Suppose we are trying to predict the class of a new
# observation 𝒙new. If a training observation 𝒙𝑖
# is far from it in terms of the Euclidean distance, i.e. ||𝒙new − 𝒙𝑖
# || is
# large, then 𝑘 (𝒙new, 𝒙𝑖) is small and thus it plays little role in the prediction 𝑔ˆnew.
# • sigmoid kernel: 𝑘 (𝒙𝑖
#                       , 𝒙 𝑗) = tanh(𝛾𝒙
#                                       ⊺
#                                       𝑖
#                                       𝒙 𝑗 + 𝑟), where 𝛾 and 𝑟 are two constants and tanh is hyperbolic tangent
# function. This sigmoid kernel comes from the neural network field and an SVM with a sigmoid kernel is equivalent
# to a 2-layer neural network (which will be introduced next week)


set.seed(136)
#Here we will look at a toy example which attempts to use a SVM to detect whether a point is inside a
#circle with radius
# Generate bivariate X from U[-1,1] x U[-1,1]
n <- 100
X <- 1-2*matrix(runif(2*n), ncol=2)
Y <- X[,1]^2+X[,2]^2+.2*rnorm(n)>sqrt(0.5)
# Plot the data
plot(X, col=2+Y, xlab="x1", ylab="x2")
# Assemble data into data frame
train.data <- data.frame(Y=Y, X=X)
# Load required R library
library(e1071)
# Fit SVM (Gaussian kernel = "radial" in e1071)
my.svm <- svm(Y~., data=train.data, type="C-classification", kernel="radial",
              cost=10, gamma=0.1)
# Create new data (grid on [-1,1])
basis <- seq(-1, 1, len=100)
test.data <- expand.grid(X.1=basis, X.2=basis)
# Get predictions from SVM
predictions <- predict(my.svm, test.data)
# Plot predictions
image(basis, basis, matrix(unclass(predictions), nrow=length(basis), byrow=TRUE),
      col=c(rgb(0.9,0.5,0.5),rgb(0.6,0.9,0.5)),xlab="x1",ylab="x2")
# Add training data
points(X, col=2+Y)
# Add filled points to identify the support vectors
points(X[my.svm$index,], col=2+Y[my.svm$index], pch=20)
# Add line corresponding to truth
t <- seq(0, 2*pi, len=100)
lines(sqrt(0.5)*sin(t), sqrt(0.5)*cos(t))
#The red class region that the SVM estimates is not too far away from the true region given by the black
#ellipse. Not too bad a job given the data we had.

#tuning svm
set.seed(3)
tune.res<-tune(svm, Y~., data=train.data, type="C-classification", kernel="radial",
               ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.res)
#We can see that with 5 choices for each of the two parameters, we get all 25 combinations produced
#along with the average error and dispersion. We can see that the best combination of parameters is 1 for
#cost and 0.5 for gamma which gives an average cross-validated error of 0.2. The actual best SVM model
#is saved in the output as an element called best.model

tune.res$best.model


#Example
C.sel<- tune.svm(sp~.,data=train.data,type="C-classification",kernel="linear",
                 cost=c(0.1,0.5,1,2,5,10))$best.parameters
final.svm<-svm(sp~.,data=train.data,kernel="linear",cost=C.sel,
               type="C-classification",decision.values=T)
fitted.train<-attributes(predict(final.svm,train.data,
                                 decision.values = TRUE))$decision.values
#But we would prefer to see the performance on the validation data
fitted.valid<-attributes(predict(final.svm,valid.data,
                                 decision.values = TRUE))$decision.values
#Plot the ROC for the validation data
library(ROCR)
pred <- prediction(fitted.valid,valid.data$sp,label.ordering=c("O","B"))
#label.ordering: change the default ordering of the classes
#by supplying a vector containing the negative and the positive class label
perf <- performance(pred, "tpr", "fpr")
plot(perf, main="Validation data ROC", cex.main=0.75, cex.lab=0.75)

AUC <- performance(pred, "auc")
AUC@y.values[[1]]