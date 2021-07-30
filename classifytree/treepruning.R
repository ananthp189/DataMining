library(plyr)
library(skimr)
library(knitr)
library(ggplot2)
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

my_skim <- skim_with(base=sfl(n=length),factor=sfl(ordered=NULL),
                     numeric=sfl(p0=NULL,p100=NULL,hist = NULL))
knit_print(my_skim(train))


library(gmodels)
CrossTable(train$Creditability, train$Account_balance,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)


#stopping criteria examples for tree
install.packages("ggridges");library(ggridges)
# Replace the four question marks with the correct variables and data set
library(ggplot2);library(ggridges)
ggplot(train, aes(x=Credit_amount, y=Purpose, fill=Creditability)) +
  geom_density_ridges(alpha=0.5) +
  theme_ridges()

library(rpart);library(rpart.plot) #load the rpart package
first_tree <- rpart(Creditability~Account_balance+Age, data=train, method="class")
#use method="class" for factors,
#i.e. for a classification tree rather than a regression tree
rpart.plot(first_tree,type=2,extra=4)

first_tree
#rrpart uses the Gini impurity measure to select splits when performing classification. If you want to use entropy as
#the measure, you can specify it in the parms argument and set split to information.

second_tree <- rpart(Creditability~Account_balance+Age, data=train, method="class",
                     parms = list(split='information'))
rpart.plot(second_tree,type=2,extra=4)
#An introduction to Recursive Partitioning using the RPART routines

#We could change the number of observations that must exist in a node in order for a split to be attempted
#by writing
third_tree <- rpart(Creditability~Account_balance+Age, data=train, method="class",
                    minsplit=100)
rpart.plot(third_tree,type=2,extra=4)
#Compared to first_tree in Example 2, this is a smaller tree with less splits. It makes sense since we
#only allow splits to happen when there are at least 100 observations in that node.
#If we want a tree to have at most three consecutive splits, we would set maxdepth to the value 3.

complex_tree_maxdepth <- rpart(Creditability~Account_balance+Age+Purpose+Credit_amount,
                               data = train, method = "class", maxdepth=3)
rpart.plot(complex_tree_maxdepth,type=2,extra=4)
#By default, the complexity parameter is 0.01. To grow a larger tree, we will need to decrease its value.
first_tree_grown <- rpart(Creditability~Account_balance+Age, data=train,
                          method="class", cp=0.004)
rpart.plot(first_tree_grown,type=2,extra=4)
#This is not  a good idea since it will typically produce overfitted trees.


#Pruning without stopping criteria
set.seed(1)
second_tree_fully_grown <- rpart(Creditability~Age+Length_of_cur_employment
                                 +Purpose, data=train, method="class",
                                 parms=list(split='information'),
                                 cp=-1, minsplit=2, minbucket=1)
#rpart.plot(second_tree_fully_grown) #Try this if you want see how it looks
printcp(second_tree_fully_grown)

complex_tree_maxdepth_penalty <- rpart(Creditability~Account_balance+Age
                                       +Purpose+Credit_amount,
                                       data=train, method="class",
                                       maxdepth=3, parms=list(loss=lossmatrix))
rpart.plot(complex_tree_maxdepth_penalty,type=2,extra=4)

second_tree_pruned <- prune(second_tree_fully_grown, cp=.007)
rpart.plot(second_tree_pruned)

#We construct a matrix with 0's on the diagonal and losses on the off-diagonal.
#The penalty for misclassifying class 1 (rejection) as class 2 (approval) is set to 4
#while misclassifying class 2 as class 1 is set to 1.
lossmatrix <- matrix(c(0,4,1,0), byrow=TRUE, nrow=2)
lossmatrix

complex_tree_maxdepth_penalty <- rpart(Creditability~Account_balance+Age
                                       +Purpose+Credit_amount,
                                       data=train, method="class",
                                       maxdepth=3, parms=list(loss=lossmatrix))
rpart.plot(complex_tree_maxdepth_penalty,type=2,extra=4)


second_tree_pruned$variable.importance



#example for prediction using trees
test <- read.csv("German_test.csv")
test$Account.Balance <- as.factor(test$Account.Balance)
test$Sex...Marital.Status <- as.factor(test$Sex...Marital.Status)
test$Type.of.apartment <- as.factor(test$Type.of.apartment)
test$Purpose <- as.factor(test$Purpose)
test$Length.of.current.employment <- as.factor(test$Length.of.current.employment)
test$Creditability <- as.factor(test$Creditability)
test$Creditability <- revalue(test$Creditability, c("0"="No_Credit","1"="Credit"))
test <- test[,c(-1,-5,-13,-14,-19,-21)]
colnames(test)[c(2,3,5:9,11:16)] <- c("Account_balance","Duration_of_credit",
                                      "Credit_amount","Value_savings_stocks","Length_of_cur_employment",
                                      "Installment_rate_in_percent","Sex_and_marital_status","Age","Concurrent_credits",
                                      "Apartment_type","Credits_at_bank","Dependents","Foreign_worker")
newtest <- test[,c("Creditability","Age","Length_of_cur_employment","Purpose")]
newtest$CreditProb <- predict(second_tree_pruned, newdata=newtest, type="prob")
newtest$CreditClass <- predict(second_tree_pruned, newdata=newtest, type="class")
newtest$CreditProb[1:3,]

#Bagging

library(purrr); set.seed(1)
data <- seq(from = 1, to = 10)
# sample without replacement
rerun(3, sample(data, replace = FALSE))
rerun(3, sample(data, replace = TRUE))


set.seed(1)
data <- rnorm(50,5,3)
head(data)
resamples <- lapply(1:1000, function(i) sample(data, replace = TRUE))
resamples[1]
r.median <- sapply(resamples, median)
head(r.median)
printcp(second_tree_fully_grown)
sd(r.median)
hist(r.median, main="Histogram of median", cex.main=0.6, cex.lab=0.6)

#To implement bagging, we will use the function randomForest from the randomForest package and specify that the
#number of variables tried at each split, mtry, should be equal to the number of variables in the model; the reason for
#setting this value will be made clearer in the next section.

#RANDOM FORES
library(randomForest); set.seed(11)
bag_train <- randomForest(Creditability~Account_balance+Age+Credit_amount+Purpose
                          +Dependents+Apartment_type,data=train,mtry=6,ntree=200)
bag_train
predictions_bag <- predict(bag_train, test, "class")
overall_class_rate_bag <- mean(predictions_bag==test$Creditability)
set.seed(11)
single_tree <- rpart(Creditability~Account_balance+Age+Credit_amount+Purpose
                     +Dependents+Apartment_type,data = train)
predictions <- predict(single_tree, test, "class")
overall_class_rate_tree <- mean(predictions==test$Creditability)
print(c(overall_class_rate_bag,overall_class_rate_tree))

library(forcats);library(ggplot2);library(caret);library(dplyr)
bag_df <- data_frame(var = rownames(randomForest::importance(bag_train)),
                     MeanDecreaseGini = randomForest::importance(bag_train)[, 1]) %>%
  mutate(var = fct_reorder(var, MeanDecreaseGini, median))
bag_ggplot <- ggplot(bag_df, aes(var, MeanDecreaseGini)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       subtitle = "Bagging", x = NULL, y = "Average decrease in the Gini Index") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        plot.subtitle = element_text(hjust = 0.5))

library(forcats);library(ggplot2);library(caret);library(dplyr)
bag_df <- data_frame(var = rownames(randomForest::importance(bag_train)),
                     MeanDecreaseGini = randomForest::importance(bag_train)[, 1]) %>%
  mutate(var = fct_reorder(var, MeanDecreaseGini, median))
bag_ggplot <- ggplot(bag_df, aes(var, MeanDecreaseGini)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       subtitle = "Bagging", x = NULL, y = "Average decrease in the Gini Index") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        plot.subtitle = element_text(hjust = 0.5))

predictions_bag <- predict(bag_train, test, "class")
overall_class_rate_bag <- mean(predictions_bag==test$Creditability)




set.seed(11)
rf_train <- randomForest(Creditability~Account_balance +Age+Credit_amount+Purpose
                         +Dependents+Apartment_type, data = train, ntree = 200)
rf_train

library(tidyr)
rf_df <- data_frame(var = rownames(randomForest::importance(rf_train)),
                    `Random forest` = randomForest::importance(rf_train)[,1]) %>%
  left_join(data_frame(var = rownames(randomForest::importance(rf_train)),
                       Bagging = randomForest::importance(bag_train)[,1])) %>%
  mutate(var = fct_reorder(var, Bagging, median)) %>%
  gather(model, gini, -var)
rf_ggplot <- ggplot(rf_df,aes(var, gini, color = model, shape=model)) +
  geom_point() + coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       x = NULL,y = "Average decrease in the Gini Index",
       color = "Method",shape="Method") +
  theme(plot.title = element_text(hjust = 0.5))


set.seed(11)
single_tree <- rpart(Creditability~Account_balance+Age+Credit_amount+Purpose
                     +Dependents+Apartment_type,data = train)
predictions <- predict(single_tree, test, "class")
overall_class_rate_tree <- mean(predictions==test$Creditability)
print(c(overall_class_rate_bag,overall_class_rate_tree))

