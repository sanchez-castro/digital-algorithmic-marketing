# Digital and Algorithmic Marketing
# Session 3, April 10 2017
# Instructor: Sanjog Misra
# Topic: Recommendation Systems

# This script is based on https://github.com/pedroconcejero
# http://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

# if needed: install.packages("recommenderlab)
# install.packages("proxy") first

library(recommenderlab)

# A Real Dataset
data(MovieLense)  # Not sure why there is an e at the end
MovieLense

## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))
## number of ratings per movie
hist(colCounts(MovieLense))
## mean rating (averaged over users)
mean(rowMeans(MovieLense))


# Training the recommender

train <- MovieLense[1:900]

dim(train) # Using 900 Raters
u <- MovieLense[901] # Score the next one

u #1 x 1664 rating matrix of class ‘realRatingMatrix’ with 124 ratings

as(u, "matrix") # peek

# Creating a recommender
# A recommender is created using the creator functionRecommender(). 
# Available recommendation methods are stored in a registry. 
# The registry can be queried. Here we are only interested
# in methods for real-valued rating data.

recommenderRegistry$get_entries(dataType = "realRatingMatrix")

# IBCF item-based collaborative filtering (real data)
# PCA based on PCA approximation (real data)
# POPULAR based on item popularity (real data)
# RANDOM
# SVD based on SVD approximation (real data)
# UBCF based on user-based collaborative filtering (real data)

# user based collaborative filtering (UBCF)
r <- Recommender(train, method = "UBCF") 
r  # object of type "Recommender"

# The model can be obtained from a recommender using getModel().
names(getModel(r))

recom <- predict(r, u, n = 5)
recom

# Convert to list
as(recom, "list")

# Evaluate Various Algorithms

scheme <- evaluationScheme(train, 
                           method = "cross", 
                           k = 2,
                           given = 10, 
                           goodRating=3)

algorithms <- list(`random items` = list(name = "RANDOM", param = NULL),
                   `popular items` = list(name = "POPULAR", param = NULL),
                   `user-based CF` = list(name = "UBCF",
                                          param = list(method = "Cosine", nn = 50)),
                   `item-based CF` = list(name = "IBCF",
                                          param = list(method = "Cosine", k = 50)))

results <- evaluate(scheme, algorithms,
                    n = c(1, 5, 10, 15, 50,200,1000))

plot(results, annotate = c(1, 3), legend = "right")

#################
# Using a Poularity based engine

r2 <- Recommender(train, method = "POPULAR") 
r2  

# Recommendations as ‘topNList’ for 1 users.
getModel(r2)$topN
topSet = as(getModel(r2)$topN, "list")

top10 = data.frame(topSet[[1]][1:10])
top10

# Documentation of this package is kinda weak
# here are some to try...
# uu.rec=Recommender(data=r[1:5], 
#                    method="UBCF", 
#                    param=list(normalize="Z-score", 
#                               method="pearson", 
#                               nn=50, 
#                               minRating=3, 
#                               sample=F)


# Predicting for a new user
# LEts us UBCF

r2 <- Recommender(train, method = "UBCF")

# Set up data for new suer

newu = as(u,"matrix")
newu[1,] = NA # FIll with blanks

# Now get some data from user
samp = sample(1:ncol(newu),100)
m=1
i=0
# Increase number of movies rated to get accuracy
NR=5
# Storage
rat = rep(0,NR)
movies.rated = rep(0,NR)

# Loop through Random movies to rate...
while(m<NR+1)
{
    i=i+1
    cat("Please rate the following movies... \n")
    cat("Use 1-5 rating, where 5 is good. \n")
    cat("If you havent seen the movie just hit enter... \n")
    idx = samp[i]
    cat("Movie #",m,": ",colnames(u)[idx])
    mr = readline("Rating:")
    if(mr!="")
    {
     rat[m]=as.numeric(mr)
     movies.rated[m] = idx
     m=m+1
    }
}

# Fill in new data
newu[,movies.rated] = rat
rownames(newu)="New User"
# Transform back to format that recommenderlab needs
newu = as(newu, "realRatingMatrix")

recom = predict(r2,newu,10)
print(as(recom,'list'))

recom2 = predict(r2,newu,type='ratings')
recom2 = t(as(recom2,'matrix'))
ord = order(-recom2)
cbind(rownames(recom2)[ord][1:10],recom2[ord][1:10])


