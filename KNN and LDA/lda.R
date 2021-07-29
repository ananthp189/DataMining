library(ISLR)
table(Default$default)
aggregate(Default$balance, by=list(Default$default), FUN=var) #calculate variance by group

aggregate(Default$income, by=list(Default$default), FUN=var)
library(GGally)
ggpairs(Default, columns=3:4, ggplot2::aes(colour=default,alpha=0.2))
#diagonal: density plots
#top right: Pearson correlation between variables (overall and by group)
#bottom left: scatterplot

#create training and test data
n <- nrow(Default)
set.seed(1)
ind <- sample(c(1:n),0.7*n) # 70%-30% training and test split
def.tr <- Default[ ind,-2] # remove the column student which is not of interest
def.te <- Default[-ind,-2]
#apply LDA
library(MASS)
def.lda <- lda(default~., data=def.tr)
def.lda

def.pred <- predict(def.lda,def.te)
names(def.pred)
head(def.pred$class,3)
head

acc <- mean(def.te$default == def.pred$class);1-acc #error rate
tab.pred <- table(def.te$default,def.pred$class); tab.pred #cross classification table
tab.rate <-prop.table(tab.pred,margin=1); tab.rate
#adjust the classification thresholds
def.pred.new <- ifelse(def.pred$posterior[,2]>0.1,"Yes","No")
acc <- mean(def.te$default == def.pred.new);1-acc
tab.pred <- table(def.te$default,def.pred.new); tab.pred
tab.rate <-prop.table(tab.pred,margin=1); tab.rate


library(ROCR)
pred <- prediction(def.pred$posterior[,2], def.te$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf,main="ROC curve")
AUC <- performance(pred, "auc")
AUC@y.values[[1]]

