library(MASS)
data(Boston)
Boston <- Boston[,c("medv","crim","lstat","rm","rad","chas")]
colnames(Boston) <- c("median.value","crime.rate","low.socio.status",
                      "aver.rooms","index.radial.highways","river.bounds")
dim(Boston)
summary(Boston)
#EDA
library(ggplot2);library(reshape2)
p1 <- ggplot(melt(Boston), aes(variable, value)) + geom_boxplot()
p1

p2 <- ggplot(Boston)+geom_histogram(aes(x=median.value),binwidth =1)
p2

p3 <- ggplot(Boston)+geom_histogram(aes(x=median.value),binwidth =1)+
  facet_wrap(~index.radial.highways)
p3

# min-max normalisation
maxs <- apply(Boston, 2, max)
mins <- apply(Boston, 2, min)
scaled <- as.data.frame(scale(Boston, center = mins, scale = maxs - mins))
# train-test random splitting
set.seed(84)
index <- sample(1:nrow(Boston),round(0.75*nrow(Boston)))
train_Boston<- scaled[ index,]
test_Boston <- scaled[-index,]
plot(nn_boston)

library(neuralnet);set.seed(83)
nn_boston <- neuralnet(median.value ~ crime.rate+low.socio.status+aver.rooms,
                       data=train_Boston, hidden=c(5), linear.output=TRUE)
library(devtools); install_github('fawda123/NeuralNetTools')
library(NeuralNetTools)
plotnet(nn_boston)

nn_boston_error <- sum((nn_boston$net.result[[1]]-train_Boston[,"median.value"])^2)/2
paste("SSE: ", round(nn_boston_error, 4))

#training error found, now test
nn_boston_test_output <- neuralnet::compute(nn_boston, test_Boston[,c("crime.rate",
                                                                      "low.socio.status","aver.rooms")])$net.result
nn_test_SSE <- sum((nn_boston_test_output-test_Boston[, "median.value"])^2)/2
nn_test_SSE
#default nn, now optimize/tine/improve
#increase width of nn

set.seed(83)
nn_boston_7 <- neuralnet(median.value ~ crime.rate+low.socio.status+aver.rooms,
                         data=train_Boston, hidden=c(7), linear.output=TRUE)
set.seed(83)
nn_boston_9 <- neuralnet(median.value ~ crime.rate+low.socio.status+aver.rooms,
                         data=train_Boston, hidden=c(9), linear.output=TRUE)

#Compute training and test SSE errors for each neural network
#For the one with 7 nodes
#Train SSE
nn_boston_error_7 <- sum((nn_boston_7$net.result[[1]]-train_Boston[,"median.value"])^2)/2
#Test SSE
nn_boston_test_output_7 <- neuralnet::compute(nn_boston_7, test_Boston[, c("crime.rate",
                                                                           "low.socio.status","aver.rooms")])$net.result
nn_test_SSE_7 <- sum((nn_boston_test_output_7-test_Boston[, "median.value"])^2)/2
#For the one with 9 nodes
#Train SSE
nn_boston_error_9 <- sum((nn_boston_9$net.result[[1]]-train_Boston[,"median.value"])^2)/2
#Test SSE
nn_boston_test_output_9 <- neuralnet::compute(nn_boston_9, test_Boston[, c("crime.rate",
                                                                           "low.socio.status","aver.rooms")])$net.result
nn_test_SSE_9 <- sum((nn_boston_test_output_9-test_Boston[, "median.value"])^2)/2
#We can now visualise the results using ggplot.
# Bar plot of results
library(tibble);library(ggplot2)
Regression_NN_Errors <- tibble(Network = rep(c("NN5", "NN7", "NN9"), each = 2),
                               DataSet = rep(c("Train", "Test"), time = 3),
                               SSE = c(
                                 nn_boston_error, nn_test_SSE,
                                 nn_boston_error_7, nn_test_SSE_7,
                                 nn_boston_error_9, nn_test_SSE_9))
nn_ggplot <- Regression_NN_Errors %>%
  ggplot(aes(Network, SSE, fill = DataSet)) +
  geom_col(position = "dodge") +
  ggtitle("Neural networks SSE (different number of nodes in the hidden layer)")
nn_ggplot

set.seed(84)
ten_nn_boston_5 <- neuralnet(median.value ~ crime.rate+low.socio.status+aver.rooms,
                             data=train_Boston, hidden=c(5), linear.output=TRUE,
                             rep=10)
plot(ten_nn_boston_5,rep="best",cex=0.7)

ten_nn_boston_test_output <- neuralnet::compute(ten_nn_boston_5,
                                                test_Boston[, c("crime.rate","low.socio.status","aver.rooms")],
                                                rep=which.min(ten_nn_boston_5$result.matrix[1,]))$net.result
ten_nn_boston_test <- sum((ten_nn_boston_test_output-test_Boston[, "median.value"])^2)/2
ten_nn_boston_test

#use different activation function
softplus <- function(x) log(1+exp(x))
set.seed(84) #set.seed(83) does not converge within the stepmax
nn_boston_5s <- neuralnet(median.value ~ crime.rate+low.socio.status+aver.rooms,
                          data=train_Boston, hidden=c(5), linear.output=TRUE,
                          act.fct=softplus)
#Train SSE
nn_boston_error_5s <- sum((nn_boston_5s$net.result[[1]]-train_Boston[,"median.value"])^2)/2
#Test SSE
nn_boston_test_output_5s <- neuralnet::compute(nn_boston_5s, test_Boston[, c("crime.rate","low.socio.status","aver.rooms")])$net.result
nn_test_SSE_5s <- sum((nn_boston_test_output_5s-test_Boston[, "median.value"])^2)/2
#Again, we will visualise the results using a similar code as before.
Regression_NN_Errors <- tibble(Network = rep(c("NN5_logistic","NN5_softplus"),each=2),
                               DataSet = rep(c("Train","Test"), time=2),
                               SSE = c(
                                 nn_boston_error, nn_test_SSE,
                                 nn_boston_error_5s, nn_test_SSE_5s))
nn_ggplot_activation <- Regression_NN_Errors %>%
  ggplot(aes(Network, SSE, fill = DataSet)) +
  geom_col(position = "dodge") +
  ggtitle("Neural networks SSE (different activation functions)")
nn_ggplot_activation

nn_garson <- garson(ten_nn_boston_5)
nn_garson

#DEEEP NEURAL NETWORKS
#example continued from classification trees 
library(plyr)
train <- read.csv("German_train.csv")
train <- train[,c(-1,-5,-13,-14,-19,-21)]
train$Account.Balance <- as.factor(train$Account.Balance)
train$Sex...Marital.Status <- as.factor(train$Sex...Marital.Status)
train$Type.of.apartment <- as.factor(train$Type.of.apartment)
train$Purpose <- as.factor(train$Purpose)
train$Length.of.current.employment <- as.factor(train$Length.of.current.employment)
train$Creditability <- as.factor(train$Creditability)
train$Creditability <- revalue(train$Creditability, c("0"="No_Credit","1"="Credit"))
colnames(train)[c(2,3,5:9,11:16)] <- c("Account_balance","Duration_of_credit",
                                       "Credit_amount","Value_savings_stocks","Length_of_cur_employment",
                                       "Installment_rate_in_percent","Sex_and_marital_status","Age","Concurrent_credits",
                                       "Apartment_type","Credits_at_bank","Dependents","Foreign_worker")
credit_card_matrix <- model.matrix(~Account_balance+Purpose, data=train)
credit_card_matrix_final <- credit_card_matrix[,-1]
head(credit_card_matrix_final,4)
head(train[,c("Account_balance","Purpose")],4)
train$Creditability <- as.integer(train$Creditability)-1
predictor_list <- paste(colnames(credit_card_matrix_final),collapse="+")
predictor_list
f <- paste(c("train$Creditability~",predictor_list),collapse="")
f


library(neuralnet);set.seed(84)
nn_credit <- neuralnet(f, data=credit_card_matrix_final, hidden=c(2),
                       linear.output=FALSE, err.fct='ce')
library(NeuralNetTools)
plotnet(nn_credit)

Credit_train_loss_nn <- nn_credit$result.matrix[1,1]
paste("CE loss function from neural network:", round(Credit_train_loss_nn,3))

Credit_train_loss_nn <- nn_credit$result.matrix[1,1]
paste("CE loss function from neural network:", round(Credit_train_loss_nn,3))
  

#GOING DEEPER:
set.seed(84)
nn_credit <- neuralnet(f,data=credit_card_matrix_final, hidden=c(2),
                       linear.output=FALSE,err.fct='ce',likelihood=TRUE)
nn_credit_two_layers_1 <- neuralnet(f,data=credit_card_matrix_final,hidden=c(1,2),
                                    linear.output=FALSE,err.fct='ce',likelihood=TRUE)
nn_credit_two_layers_2 <- neuralnet(f,data=credit_card_matrix_final,hidden=c(2,1),
                                    linear.output=FALSE,err.fct='ce',likelihood=TRUE)
nn_credit_two_layers_3 <- neuralnet(f,data=credit_card_matrix_final,hidden=c(5,5),
                                    linear.output=FALSE,err.fct = 'ce',likelihood=TRUE)
library(dplyr)
Class_NN_ICs <- tibble('Network' = rep(c("NN_single_2","NN_two_12", "NN_two_21",
                                         "NN_two_55"), each = 3),
                       'Metric' = rep(c('AIC', 'BIC','CE Loss'), length.out=12),
                       'Value' = c(nn_credit$result.matrix[4,1],
                                   nn_credit$result.matrix[5,1],
                                   nn_credit$result.matrix[1,1],
                                   nn_credit_two_layers_1$result.matrix[4,1],
                                   nn_credit_two_layers_1$result.matrix[5,1],
                                   nn_credit_two_layers_1$result.matrix[1,1],
                                   nn_credit_two_layers_2$result.matrix[4,1],
                                   nn_credit_two_layers_2$result.matrix[5,1],
                                   nn_credit_two_layers_2$result.matrix[1,1],
                                   nn_credit_two_layers_3$result.matrix[4,1],
                                   nn_credit_two_layers_3$result.matrix[5,1],
                                   nn_credit_two_layers_3$result.matrix[1,1]
                       ))
nn_ggplot <- Class_NN_ICs %>%
ggplot(aes(Network, Value,fill=Metric)) +
  geom_col(position = 'dodge') +
  ggtitle("AIC, BIC, and cross-entropy losses of the neural networks")
nn_ggplot


#multinomial or softmax regression
set.seed(84)
alpha <- 0.7 # percentage of training set
inTrain <- sample(1:nrow(iris), round(alpha * nrow(iris)))
train.set <- iris[ inTrain,]
test.set <- iris[-inTrain,]
library(nnet)
trainData <- cbind(train.set[, 1:4], class.ind(train.set$Species))
head(trainData)
testData <- cbind(test.set[, 1:4], class.ind(test.set$Species))
set.seed(84)
nn_iris <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length +
                       Sepal.Width + Petal.Length + Petal.Width,
                     data=trainData, hidden=c(2,2), linear.output=FALSE,
                     err.fct='ce', likelihood=TRUE)
plotnet(nn_iris)

original_values <- max.col(trainData[,5:7])
predictions <- max.col(nn_iris$net.result[[1]])
mean(original_values==predictions)

compute_test <- neuralnet::compute(nn_iris,testData[, 1:4])
predictions_test <- max.col(compute_test$net.result)
original_values <- max.col(testData[,5:7])
mean(original_values==predictions_test)


library(VGAM)
fit.MLR <- vglm(Species ~ Sepal.Length + Sepal.Width
                + Petal.Length + Petal.Width, family=multinomial,data=train.set)
predict_test_MLR <- max.col(predict(fit.MLR,test.set[,1:4], type="response"))
predict_train_MLR <- max.col(predict(fit.MLR,train.set[,1:4],type="response"))
original_values_test <- max.col(testData[,5:7])
original_values_train <- max.col(trainData[,5:7])
mean(original_values_train==predict_train_MLR)
