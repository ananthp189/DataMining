set.seed(101)

alpha     <- 0.8 # percentage of training set

inTrain   <- sample(1:nrow(iris), alpha * nrow(iris))

train.set <- iris[inTrain,]

test.set  <- iris[-inTrain,]