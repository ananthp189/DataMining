#pca regression

n <- 100
x <- sort(runif(n)) # Create 100 values between 0 and 1.
y <- 5*x^2 - x + 0.1*rnorm(n) # Create simulated response.
plot(x,y) # Plot the data.
17
reg.model <- lm(y~x+I(x^2)) # Fit the regression model.
lines(x,fitted(reg.model),col="red") # Plot the fitted function.

#What if we change the range of the 𝑥𝑖 from [0, 1] to [1000, 1001]?


n <- 100
x <- sort(runif(n)) # Create 100 values between 0 and 1.
y<-(5*x)^2-x+0.1*rnorm(n) # Create simulated response.
plot(x,y) # Plot the data.
reg.model <- lm(y~x+I(x^2)) # Fit the regression model.
lines(x,fitted(reg.model),col="red")
z <- x+1000 # Add 1000 to each x.
plot(x,y) # Plot the data.
reg.model <- lm(y~z+I(z^2)) # Fit the regression model.
lines(x,fitted(reg.model),col="red") # Plot the fitted function.
#I the second model, R clearly gets the computation of the regression line wrong. This is not a bug in R,
#but due to numerical difficulties.
eigen(crossprod(cbind(1,x,x^2)))$values
eigen(crossprod(cbind(1,z,z^2)))$value


prostate <- read.csv("prostate.csv");pairs(prostate,lower.panel = NULL)
mod<-lm(lpsa~.,data=prostate)

#recall the ~. notation means use all other variables apart from lpsa

mod
#Next we run a PCA on the explanatory variables and use the scores as input to a new regression model.
#we remove the 9th variable which is our outcome
prostate.pca<-princomp(prostate[,-9])
mod.pcr<-lm(prostate$lpsa~prostate.pca$scores)
coef(mod.pcr)
#Comparing the coefficients on each, we see vastly different coefficients, because of course, despite having
#the same outcome and using the same data, they are two different regressions
t(prostate.pca$loadings%*%mod.pcr$coefficients[-1])

#var bias tradeoff
p<-ncol(prostate)-1 #we "lose" one of the outcome variables
MSE<-numeric(p)
prostate.pca<-princomp(prostate[,-9])
for (q in 1:p) {
  temp<-lm(prostate$lpsa~prostate.pca$scores[,1:q,drop=F])
  MSE[q]<-sum((prostate$lpsa-temp$fitted)^2)
}
which.min(MSE)
plot(c(1:p),MSE,type="b",xlab="No. of PCs")
#The best MSE is obtained for 8 PCs, the same as the number of the original explanatory variables. So no
#dimension reduction or regularisation takes place in this case.
#If we looked at the % of variability and chose the number of components to represent 99% of the original
#variability and ran a regression on this subset we would get the following:

n_pc <- which(cumsum((prostate.pca$sdev^2)/sum(prostate.pca$sdev^2))>0.99)[1]
mod.reduced<-lm(prostate$lpsa~prostate.pca$scores[,1:n_pc])
t((prostate.pca$loadings[,1:n_pc])%*%mod.reduced$coefficients[-1])
mod.full<-lm(lpsa~.,data=prostate)
mod.full
