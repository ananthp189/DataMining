#Recommender systems

#Two approaches
#1) content based Filtering based on prior
#2) COllaborative based on other similar users

#implemented using user preference matrix
#1)
#Profile both user and content firs

library(recommenderlab)
data("MovieLense")
## look at the first five ratings of the first user
head(MovieLense@data[1,],5)


## visualise part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
summary(rowCounts(MovieLense))

## mean rating (averaged over users)
mean(Matrix::rowMeans(MovieLense))
hist(Matrix::rowMeans(MovieLense), main="Histogram of mean user rating")
## number of ratings per movie
summary(colCounts(MovieLense))

## available movie meta information
head(MovieLenseMeta,3)

## available user meta information
head(MovieLenseUser)

rec_mod <- Recommender(MovieLense, method="UBCF", param=list(method="pearson",nn=10))
#Obtain top 5 recommendations for 1st user entry in dataset
Top_5_pred <- predict(rec_mod, MovieLense[1], n=5)
Top_5_List <- as(Top_5_pred, "list")
Top_5_List

recom_svdf <- Recommender(MovieLense, method="SVDF", param=list(k=2))
pred_rating <- predict(recom_svdf, MovieLense, type="ratings")
pred_rating@data[1:5,1:5]

#evaluate the two models created
set.seed(1)
evlt <- evaluationScheme(MovieLense, method="split",
                         train=0.7, given=15)
evlt
tr <- getData(evlt, "train"); tr
te_known <- getData(evlt, "known"); te_known
te_unknown <- getData(evlt, "unknown"); te_unknown
#create two recommenderw (user collaborative and uv decomp)

r1 <- Recommender(tr, "UBCF")
r2 <- Recommender(tr, "SVDF")

p1 <- predict(r1, te_known, type="ratings")
p2 <- predict(r2, te_known, type="ratings")
error <- rbind(UBCF=calcPredictionAccuracy(p1, te_unknown),
               SVDF=calcPredictionAccuracy(p2, te_unknown))
error

#Evaluate function to compare many recommender together
algs <- list(
  "user-based CF" = list(name="UBCF", param=NULL),
  "UV-decomposition" = list(name="SVDF", param=NULL))
results <- evaluate(evlt, algs, type="ratings")
rbind(getResults(results$`user-based CF`)[[1]],
      getResults(results$`UV-decomposition`)[[1]])
recommenderlab::plot(results,legend="topright")


set.seed(1)
evlt2 <- evaluationScheme(MovieLense, method="split",
                          train=0.7, given=15, goodRating=4)
#goodRating: threshold at which ratings are
#considered good for evaluation
results2 <- evaluate(evlt2, algs, type="topNList", n=10)
#create a list with top 10 items
rbind(getResults(results2$`user-based CF`)[[1]],
      getResults(results2$`UV-decomposition`)[[1]])

library(arules)
data("Groceries")
15
inspect(head(Groceries,3))
groceries <- as(Groceries,"binaryRatingMatrix")
scheme <- evaluationScheme(groceries,train=0.7,given=-1)
AR <- evaluate(scheme, "AR", type="topNList", n=1,
               param=list(support=0.01,confidence=0.01))
getResults(AR)
