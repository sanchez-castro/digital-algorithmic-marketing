# Simple K-Means Custering Example
# Simulate Data
# Observations
N = 100
# Clusters
K = 4
# Seed
set.seed(K)
# Generate Data
dat = data.frame(x = c(sapply(1:K, function(i) rnorm(N/K, runif(1)*(i+2)^2,5))), 
                y = c(sapply(1:K, function(i) rnorm(N/K, runif(1)*(i+2)^2,5))),
                z = c(sapply(1:K, function(i) rnorm(N/K, runif(1)*(i+2)^2,5))),
                clust = rep(1:K, each=N/K) )

plot(y=dat$y,x=dat$x,col=dat$clust,pch=19,xlab="X",ylab="Y",type="n")
grid(lty=2,col='darkgrey')
points(y=dat$y,x=dat$x,col=dat$clust,pch=19)

# Animation
library(animation)
oopt = ani.options(interval = 2)
set.seed(123)
kmeans.ani(x=dat[,1:2],centers=4,hints = c("Move centers!", "Find cluster?"), pch = 18:21, col = 1:4)

# Production version of k-means is included in R!
# K Means Clustering
out = kmeans(dat[,1:3],centers=4,nstart=20)
plot(dat[,1:2],col=out$cluster,pch=19,xlab="X",ylab="Y")
points(out$centers,col="white",pch=20,cex=2)
points(out$centers,col=1:K,pch=19,cex=1)


# Construct measures
# Fitted (Just the cluster center ) 
X = dat[,1:2]
fitted.X = fitted(out)
head(fitted.X)

# Deviations from the center
resid.X = X - fitted.X

# Summary Stats
res.table = cbind(out[c("betweenss", "tot.withinss", "totss")])
rownames(res.table) = c("between.ss", "tot.withinss", "totss")
colnames(res.table)="Estimate"
print(res.table)

# We can also examine within by cluster 
out$withinss

# Remember the whole idea is to minimize within cluster variation
# and maximize between cluster variation!

# How many clusters?
# One way is to use the Elbow plot
mydata <- dat[,1:2]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i,nstart=4)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




