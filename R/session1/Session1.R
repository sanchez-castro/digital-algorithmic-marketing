# R Code for Session 1 - Part I

# Simulated Display Advertising Experiment
# Simulating Consumers
set.seed(543210)
# How Many consumers in experiment?
N=200
# Display Adv Schedule for "Experiment"
disp.sch = 0:7
# Expected number of consumers in each disp bin
Q = N/length(disp.sch)
# Simulated ads
disp = sample(disp.sch,N,replace=TRUE)

#Consumer Response
# Construct Utilities
util = -5+2*disp
# Choice Probabilities
prob = 1/(1+exp(-util))
# Choices
buy = rbinom(N,1,prob)

# Let's assume these were obtained based on an experiement
# This will become real in weeks to come. (wink. wink.)
# Elementary Demand Analysis
# Demand Schedule
dem.sch = table(disp,buy)
par(mfrow=c(2,1))
plot(x=disp.sch,y=dem.sch[,2],type='b',pch=19,cex=1.5,xlab="Price",ylab="Demand")

# Task: Construct a Profit Schedule and plot
# Assume each display ad costs $1 and a purchase nets us $10 in margin
profits = 10*dem.sch[,2]-rowSums(dem.sch)*disp.sch
plot(x=disp.sch,y=profits,type='b',pch=19,cex=1.5,xlab="Price",ylab="Profits")

# Discussion
# What does this tell you?
# Any comments/thoughts/ideas?

# Estimation
# Logit Model
res = glm(buy~disp,family=binomial)
summary(res)
# Coefficients
cfs = coef(res)
cfs
# Profit Function
profit = function(disp)
{
  # Construct Choice Model
  util = cfs[1]+cfs[2]*disp
  prob = 1/(1+exp(-util))
  # Profits
  profit = Q*sum(10*prob-disp)
  return(profit)
}
# Allow Vectorization of function
profit.func = Vectorize(profit)
# Plot
par(mfrow=c(1,1))
curve(profit.func,0,10,col='steelblue',lwd=3)
# Overlay
lines(x=0:9,profit.func(0:9),col='blue')
lines(x=disp.sch,y=profits,type='b',pch=19,cex=1.5,xlab="Price",ylab="Profits",col='darkgrey',lwd=3)

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
# Would you trust the results?
# What would you do differently?

