# Digital and Algorithmic Marketing
# Session 5, April 24 2017
# Instructor: Sanjog Misra
# Topic: Optimal Design and Content Optimization
 
# Create a Design Matrix
# install.packages("AlgDesign") 
library(AlgDesign)


# Simple Linear Regression
#Points to learn from the function
NP=1000
x = seq(0,1,length=NP)
b = rnorm(1,1,2)

fx = function(x)  b[1]*x
y = fx(x) + .05*rnorm(NP)

# we created 1000 points that fits a linear model with coefficient b[1] with noise from a normal dist
plot(y)

#creating a dataframe from the sequence from 0-1
xdat = data.frame(x=x)
xdat

#true function we want to learn
curve(fx, col= "red")

points(y~x)

NT =2
#There is no y beacuse we do not know the Y. How many trials? Just two points from xdat
des = optFederov(~x,nTrials = NT,data=xdat)
des
#column 1 and 1000 wtih values 0 and 1
des$design

# A bit more interesting regression...

NP=1000
x = seq(0,1,length=NP)
b = rnorm(3,1,2)
b
# Y is cubic function of x
fx = function(x)  b[1]*x-b[2]*x^2+b[3]*x^3

# Generate ys
y = fx(x) + .05*rnorm(NP)

curve(fx)
points(y~x)

# Find optimal Design
xdat = data.frame(x=x)

NT = 5 # Bare minimum is 4
des = optFederov(~x+I(x^2)+I(x^3), nTrials = NT, data=xdat, criterion = "D")
des

# Get design points
xp = unlist(des$design)

# Plot them
points(x=as.numeric(xp),y=xp*0,pch=19,col='red')

# Generate data at those points
yp = fx(xp) + .05*rnorm(NT)

# Run a  regression with NT points
reg = lm(yp~xp+I(xp^2)+I(xp^3))
summary(reg)
# Extract Coefficients
cf = reg$coefficients
# Create function
fx_hat = function(x) {
        cf[1]+cf[2]*x+cf[3]*I(x^2)+cf[4]*I(x^3)
}
# Check to see if we did ok
curve(fx_hat,add=TRUE,col='red',lwd=3)


# Optimal Message Design
# based on live project for CA SNAP program
# Txt message nudge to get eligible people apply for foodstamps.

V1=c("[Name],","(no name)","Hello [Name], ","Hi [Name], ")
V2=c("You already receive Medi-Cal. ",
"You've already qualified for Medi-Cal. ",
"Do you already receive Medi-Cal benefits? ",
"Do you receive Medi-Cal benefits? ")
V3=c("You may be also able to get ",
"You may also qualify for ")
V4=c("free groceries (foodstamps). ",
"money for groceries (foodstamps). ")
V5=c("To learn more ","To find out more ")
V6=c("click here.","cick Now","click this link","click this link now")
V7=c(".","!")

Vs = list(V1,V2,V3,V4,V5,V6,V7)

# Levels
levs=sapply(Vs,length)


# Full Factorial Design
mat = gen.factorial(levels=levs,varNames=paste("V",1:7,sep=""),factors="all")

head(mat)
tail(mat)

# Let's Generate data from this design
# Convert to a model matrix
mm1 = model.matrix(~.,data=mat) 
head(mm1)

# Some coefficients 
cf.mm  = c(-3,-.5,.2,.3,.5,.1,-.2,.6,-1,.05,-.1,-.1,.15,.25)

# Simulating Consumer behavior
# Utility
# This is simply beta1*X1 + ... + betak*Xk
u = mm1%*%cf.mm

# probability of clicking
prob = 1/(1+exp(-u))

# Low probability event
hist(prob)


##########################################

##########################################
# Number of Customers (in each cell)
N=1000

# Generate Choices for each "message"
set.seed(12345)
#We do not know the outputs, we create a vector of 0's and then we assign a probability
y = rep(0,length(prob))

for(j in 1:length(prob))
{
	y[j] = rbinom(1,N,prob[j])
}

# responses
hist(y)

# How many coefs are there? From the full factorial cobinations converted in the matrix
ncol(mm1)

# Can we find set of messages to test?
set.seed(61113)
ds1 = optFederov( ~ V1+V2+V3+V4+V5+V6+V7,data = mat, nTrials = 32,criterion="D")$design

# Pretend we ran the experiment
# Find those messages in our data
y.ds1 = y[as.numeric(rownames(ds1))]


# Show
barplot(y.ds1/N)


# Now build a model with that subset
Xm = model.matrix(~.,ds1)

# note the weird specification
# y.tab is successes and total trials
ytab = cbind(y.ds1,N-y.ds1)


# We can run a logit with that
sm.ds1 = glm(ytab~Xm-1,family='binomial') # No intercept
summary(sm.ds1)



# Does this even matter?
# What if we just took 16 random rows
ds0 = mat[sample(1:1024,16),]
y.ds0 = y[as.numeric(rownames(ds0))]
mm.ds0 = model.matrix(~.,ds0)
sm.ds0 = summary(glm(cbind(y.ds0,N-y.ds0)~mm.ds0-1,family='binomial'))

#Compare
cbind(coef(summary(sm.ds1))[,1:2],coef(sm.ds0)[,1:2],cf.mm)

# Plot 
plot(coef(sm.ds1),cf.mm,pch=19,col='orange',cex=1.5,ylab="True parameters",xlab="Estimated Parameters")
points(coef(sm.ds0)[,1],cf.mm,pch=19,col='magenta',cex=1.5,)
legend('bottomright',legend=c("Random","Optimal"),col=c("magenta","orange"),pch=c(19,19))
abline(a=0,b=1,lty=2,col='red')

# Now we can score
newdat = data.frame(Xm=I(model.matrix(~.,data=mat)))
score = predict(sm.ds1,newdata=newdat)


# What's the message
idx = which.max(score)
opt.mess = as.numeric(mat[idx,])

# Example
message = ""
for(j in 1:7){
    message=paste(message,c(Vs[[j]])[opt.mess[j]],sep="")
}

message
