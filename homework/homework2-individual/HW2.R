# Digital and Algorithmic Marketing
# Homework #2 (Individual HW)
# Instructor: Sanjog Misra
# Topic: Recommendation Systems

# if needed: install.packages("recommenderlab)

library(recommenderlab)

# A Real Dataset
data(MovieLense)  # Not sure why there is an e at the end
MovieLense

# Q1: For this question use the MovieLense dataset to compare the performance of user based (UBCF) and item
#       based (IBCF) collaborative filtering models. For each part provide a short and concise answer when
#       required and show the relevant line of code.
#       a) Split the data into training and holdout samples [use 80/20 split (train/holdout) for the first 900
#           users in MovieLense database].
#       b) Compare the time it takes to fit the UBCF and IBCF models.
#       c) Predict the entire unknown ratings for the users in the holdout data using the fitted UBCF and IBCF
#           models. Compare the predictive accuracy and the time it takes to make the predictions.
#       d) Compare the predictive accuracy of these predictions. In particular, compare the overlap in the
#           predictions of the two methods. (see hints below)
#       e) Explain the intuition behind the differences you find.
# 
# Q2: Imagine that you could append movie characteristics (Budget, Genre,
#     Studio, Actors, Director etc.) to this data. How would you use this 
#     to construct a recommendation system? Outline any specifics you can 
#     about this new system.

# Hints: To evalaute timing you can use the system.time command.
#       For example, system.time(r <- Recommender(data, method = "RANDOM") )
#       will tell you how gold raining a RANDOM CF algorithm takes.
#       To assess predictive accuracy you can use the function calcPredictionAccuracy
#       See ?calcPredictionAccuracy for examples 
#       or the recommenderlab vignette: 
#       https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf  

#       Finally, to examine overlap you may use the following function
rec.overlap = function(recA,recB,i)
{
  length(intersect(recA@items[[i]],recB@items[[i]]))
}

# Example
# Split the data
# Hint: You may want to look at the Reccomderlab 
#       vignette for better ways of doing this.
train <- MovieLense[1:300]
test <- MovieLense[301:350]

# Training the recommender
r1 <- Recommender(train, method = "RANDOM") 
r2 <- Recommender(train, method = "POPULAR") 

# Predict for the test data
rec1 = predict(r1,test,n=150)
rec2 = predict(r2,test,n=150)

# How many reccomendations overlap for user 12 in the test sample
# You can do this for all the users in test
rec.overlap(rec1,rec2,i=12)


