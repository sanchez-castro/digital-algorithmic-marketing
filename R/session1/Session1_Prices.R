# Digital and Algorithmic Marketing
# Session 1, March 27 2017
# Instructor: Sanjog Misra

# Task: Advertising Response
# --------------------------------
# Simulating consumers
# --------------------------------
# Set seed for replication
set.seed(54321)
# How Many consumers in experiment?
N=200
# Lets Rando
price.sch = seq(1,10,length=10)
# Expected number of consumers  in each price bin
Q = N/length(price.sch)
# Simulated Prices
price = sample(price.sch,N,replace=TRUE)
barplot(table(price))
# Construct Utilities
util = 5-price
# Choice Probabilities
prob = 1/(1+exp(-util))
# Choices
buy = rbinom(N,1,prob)
# Let's assume these were obtained based on an experiement
# This will become real in weeks to come. (wink. wink.)

# Elementary Demand Analysis
# Demand Schedule
dem.sch = table(price,buy)
qnty = dem.scg[,2] # Is there an error? Fix it.

# Plot the Demand Curve
par(mfrow=c(2,1))
plot(x=price.sch,y=qnty,type='b',pch=19,cex=1.5,xlab="Price",ylab="Demand")

# Task: Construct a Profit Schedule and plot
# Hint: profits = (p-mc)*Demand(p)
marg.cost = 1

# profits = ??
# Write down a function that computes profit for a given price
profits = function(p)
{
    # your Code goes here...
}

# Now this prot should work.
plot(x=price.sch,y=profits,type='b',pch=19,cex=1.5,xlab="Price",ylab="Profits")

# Discussion
# What does this tell you?
# Any comments/thoughts/ideas?

# --------------------
# Estimation
# --------------------

# Logit Model
res = glm(buy~price,family=binomial)
summary(res)
# Coefficients
cfs = coef(res)
cfs
# Profit Function
profit.f = function(prc)
{
  # Construct Choice Model
  util = cfs[1]+cfs[2]*prc
  prob = 1/(1+exp(-util))
  # Profits
  profit = Q*sum((prc-marg.cost)*prob)
  return(profit)
}
# Allow Vectorization of function
profit.func = Vectorize(profit.f)
# Plot
par(mfrow=c(1,1))
curve(profit.func,1,10,col='steelblue',lwd=3)
# Overlay
lines(x=price.sch,y=profits,type='b',pch=19,cex=1.5,xlab="Price",ylab="Profits",col='darkgrey',lwd=3)

# Task: What would sales & profits be if we charged $5.50
# Hint Construct function for sales
# sales = function(prc) { ? }

# -----------------------
# Optimization
# -----------------------
# Now lets optimize!
# This is a single variable optimization so we use optimize
# For more than one we use optim
res = optimize(f = profit,lower = 0,upper=100,maximum = TRUE)
# Extract Results
p.star = res$max
profit.star = res$obj
# Plot
points(x=p.star,y=profit.star,pch=19,cex=2,col='red')
abline(v=p.star,lty=2,col='red')

# Discussion
# Is the model good?
# Would you trust the price?
# What would you do differently?

# Can we create an algorithm that takes parameters and spits out optimal prices?
