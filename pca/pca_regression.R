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

