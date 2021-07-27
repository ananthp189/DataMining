#PCA
#EXPLORE DATA FIRST , MORE CORRELATION->PCA GOOD because of redundancy (REMOVE OUTLIERS)
#princomp

library(skimr)
library(knitr)
library(corrplot)

wine <- read.csv("wine.data.csv")
str(wine, vec.len = 2)
summary(wine)
my_skim <- skim_with(base = sfl(n = length), numeric = sfl(p0 = NULL, p100 = NULL,
                                                           hist = NULL))

knit_print(my_skim(wine))
pairs(wine[,c(1:2,5:8,14)],pch=20,lower.panel = NULL)
plot(wine$Flavanoids,wine$Proline,xlab="Flanavoids",ylab="Proline")
#Use the mouse to click on the point you want and the Esc key to stop the command
outlier<-identify(cbind(wine$Flavanoids,wine$Proline))
wine.new<-wine[-122,-1]
M <- cor(wine.new);corrplot(M, method = "number",type="upper")

round(diag(var(wine.new)),2)

wine.pca<-princomp(wine.new, cor=T)
wine.pca
summary(wine.pca)
plot(wine.pca)

#kaiser
#Extract the component standard deviations
sd.pca<-wine.pca$sdev
#Find the average variance, take the mean after squaring them
ave.var<-mean((sd.pcaˆ2))
ave.var

#Find which components have higher than average variance (TRUE)
sd.pcaˆ2>ave.var
