library(plyr)
library(skimr)
library(knitr)
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



install.packages("ggridges");library(ggridges)
# Replace the four question marks with the correct variables and data set
ggplot(train, aes(x=Credit_amount, y=Purpose, fill=Creditability)) +
  geom_density_ridges(alpha=0.5) +
  theme_ridges()

library(rpart);library(rpart.plot) #load the rpart package
first_tree <- rpart(Creditability~Account_balance+Age, data=train, method="class")
#use method="class" for factors,
#i.e. for a classification tree rather than a regression tree
rpart.plot(first_tree,type=2,extra=4)

