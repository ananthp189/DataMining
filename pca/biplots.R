library(skimr)
library(knitr)
library(corrplot)
library(TeachingDemos)
library(ade4)

pairs(USArrests,pch=20,lower.panel = NULL)
data("USArrests")
str(USArrests,vec.len=2)

knit_print(skim(USArrests))
cor.arrest<-cor(USArrests)
round(cor.arrest,2)
set.seed(1)
arrest.pca<-princomp(USArrests)
summary(arrest.pca)
biplot(arrest.pca,xlim=c(-0.3,0.4),cex=.7)

biplot(arrest.pca,xlim=c(-0.3,0.4),xlabs=rep("*",nrow(USArrests)), cex=c(1,0.5),
       arrow.len=0.05)
