#####################################

#Setting the random generator seed to ensure similar responses when re-running code
set.seed(135)
##################################
#Reading in and preparing the data
##################################
#Open the library ade4 where the data is
library(ade4)
#Load the tortues dataset
data("tortues")
#Look at the first few lines of the data
head(tortues)
#Extract the females turtles data into a new dataset called fem.turt
fem.turt<-tortues[tortues[,4]=="F",-4]
20
#Take the log of all the variables in the new dataset
log.fem.turt<-log(fem.turt)
#Name the variables
colnames(log.fem.turt)<-c("log.length","log.width", "log.breadth")
##############
#Summary Plots
##############
#Create a pairsplot of the data
pairs(log.fem.turt,pch=20, lower.panel=NULL)
#Create a 3-d scatterplot of the data
library(lattice)
cloud(log.length~log.width*log.breadth,data=log.fem.turt)
#Rotate the 3-d scatterplot of the data
library(TeachingDemos)
#Use your mouse to drag the sliders to change the plot
rotate.cloud(log.length~log.width*log.breadth,data=log.fem.turt)
####################
#Numerical Summaries
####################
#Correlation matrix
round(cor(log.fem.turt),2)
#Standard deviations
apply(log.fem.turt,2,sd)
#############################
#Principal Component Analysis
#############################
pca.turt<-princomp(log.fem.turt);pca.turt
#Change princomp to prcomp and use the help page to find out loadings, scores etc.
######################
#Looking at the scores
######################
head(pca.turt$scores);plot(pca.turt$scores,pch=20)
#outlier<-identify(pca.turt$scores)
#####################################
#Run PCA on dataset excluding outlier
#####################################
pca.turt.new<-princomp(log.fem.turt[-10,]);pca.turt.new
####################################
#Deciding on number of PCs to retain
####################################
plot(pca.turt.new);summary(pca.turt.new)
sd.pca<-summary(pca.turt.new)$sdev
tot.var<-sum(sd.pcaˆ2)
ave.var<-tot.var/ncol(log.fem.turt)
ave.var
sd.pcaˆ2>ave.var
##########################
#Interpreting the loadings
##########################
pca.turt.new$loadings
21
#######################
#Calculating new scores
#######################
new.data<-data.frame(log.length=c(4.8),log.width=c(4.7),log.breadth=c(3.9))
predict(pca.turt.new,new.data)