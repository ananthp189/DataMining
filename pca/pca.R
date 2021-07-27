#PCA
#EXPLORE DATA FIRST , MORE CORRELATION->PCA GOOD because of redundancy (REMOVE OUTLIERS)
#princomp

library(skimr)
library(knitr)

wine <- read.csv("wine.data.csv")
str(wine, vec.len = 2)
summary(wine)
my_skim <- skim_with(base = sfl(n = length), numeric = sfl(p0 = NULL, p100 = NULL,
                                                           hist = NULL))

knit_print(my_skim(wine))
pairs(wine[,c(1:2,5:8,14)],pch=20,lower.panel = NULL)
