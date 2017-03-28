# Digital and Algorithmic Marketing
# Session 1, March 27 2017
# Instructor: Sanjog Misra
# Task: Intro to R

# This script covers some basic R commands
# as well as a demonstration of some more advanced
# methods. 

# R is acommand line based and interprets your code
# (as opposed to comiling them as in C/C++ etc.)
# For example you could do simple arithmetic operations ...
1 + 2 
# or
12 * 12

# we can write our own functions
myfunction = function(x,y)
{
  z = x+y 
  return(z)
}

# Once we source or run the function code
# we can call this function
myfunction(5,5)
# Should give you output 
# [1] 10

# A more complex function
myfunction2 = function(n)
{
  total = 0
  for(i in 1:n)
  {
    total = total + i
  }
  total
}

myfunction2(4)

# Simulating data
#Set seed to ensure replicability 
set.seed(1234)

# A thousand draws from a standard normal
x = rnorm(1000)

# A smooth density plot (kind of like a histogram)
plot(density(x))
hist(x, prob=TRUE, col='lightgray')
lines(density(x), col='red')
lines(density(x), col='red', lty=2, lwd=3)

# Simulate some error terms
err = rnorm(1000)

# Generate y
y = 10 + 2*x + err

# Scatter Plot
plot(y=y,x=x)

# Data.frame
df = data.frame(x=x,y=y)

# Summary Stats
summary(df)

# Regression
lm.res = lm(y~x,data=df)

# Summarize the Regression
summary(lm.res)

# Extract the coefficients
cf = coef(lm.res)

# Plot Again
plot(y~x,pch=19,col='darkgrey')

# Add fitted line
abline(a=cf[1],b=cf[2],lty=2,lwd=2,col='red')

# Basic Optimization
# Some fucntions
fx = function(x)
{
  1-x*(1-x)
}
# What does it look like?
curve(fx)

# Find Minimum
res = optimize(fx,interval=c(0,1))

# plot that point
points(x=res$minimum, y=res$objective,pch=19,col='red',cex=1.5)

res

# Multidimensional optimization
# 2 dimensional problem
fx2 = function(z)
{
  out = z[1]+z[2]-.5*(z[1]+z[2])^2
  -out
}
# This time we use the optim function in R
res2 = optim(c(0,0),fx2,method="BFGS")

res2

# So can we do our own regression?
obj = function(beta)
{
  xb = beta[1]+beta[2]*x
  uhat = y-xb
  obj = sum(uhat^2)
}

lm.out = optim(c(0,0),obj,method="BFGS")
print(lm.out)

# Compare to canned procedure result lm.res





