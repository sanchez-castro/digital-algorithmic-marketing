
setwd("C:/Booth/Digital & Algorithmic Marketing/HW1")

# Digital and Algorithmic Marketing
# Session 2, April 03, 2017
# Instructor: Sanjog Misra
# Group Homework #1

# A new DMP claims that hey have a better set of variables
# to match your customers on. 
# In particular, they have an ecom_index which they claim 
# Offers an inrementally better match for you.
# Unfortunately the cost of matching customers via them is 
# significantly higher!
#not more than 2-3 pages

cost = 3.25

# They have given you a sample target dataset 
# and a matched version of your customer dataset
# These are in cust2.rdat and target2.rdat
# Hint: Careful the ecom_index is in the 12th column

# Use this new data to answer the following questions.
# 1. Would you go with the new DMP? Justify your answer.
# 2. Is there any circumstance when you would ignore the
#    matching and go after the entire target file audience? Justify.

library(FNN)
set.seed(1234)

# Load Data
load(file="C:/Booth/Digital & Algorithmic Marketing/HW1/cust2.rdat")
load(file="C:/Booth/Digital & Algorithmic Marketing/HW1/target2.rdat")

head(cust2)
names(cust2)
summary(cust2)
nrow(cust2)

head(target2)
names(target2)
summary(target2)
nrow(target2)

#what if we target all
cost = 3.25
profit_all = sum(target2[,"spend"])-cost*length(target2[,"spend"])
profit_all
# -57,399.67

table(target2[,"spend"]>0)
#TRUE = 110

##Who are valuable customers?

###use same criteria as the class example: spend>$10
summary(cust2[cust2$spend>0,"spend"])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.53   14.14   29.94   97.37   61.12 6768.00

summary(target2[target2$spend>0,"spend"])
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.99   19.38   35.78   59.82   71.61  389.90 

cs=cust2[cust2$spend>10,c(2:10,12)]
CV = mean(cust2[cust2$spend>10,"spend"]); CV #110.135
EV = mean(cust2[,"spend"]); EV #2.9

seed=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index+ecom_index-1, data=cs)

head(seed)

ts = target2[,c(2:10,12)]
names(ts)
targ=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index+ecom_index-1, data=ts)

targ=targ[,intersect(colnames(seed),colnames(targ))]

#kNN

profit3=rep(0,20)

# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit3[k] = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
}

plot(profit3,type='b',pch=19)
title(main = "Exhibit 1: DMP Profits All Variables")
abline(h=2,lty=2)

# Max profits
# Best k
k.star = which.max(profit3); k.star #k = 2
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit3.star = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
profit3.star #-45.3

# Explore matches
# How many did we really match?
table(target2[matches,"spend"]>0)
#TRUE = 8 FALSE = 248

# What percentage of spend did we capture?
sum(target2[,"spend"]) #6,579.83
perc.captured = sum(target2[matches,"spend"])/sum(target2[,"spend"])
perc.captured #11.9%

# variable reduction through regression
reg = lm(spend~census_region 
         +household_size+hoh_oldest_age    
         +household_income+children         
         +racial_background+connection_speed  
         +country_of_origin+retail_index+ecom_index, data=cust2)
summary(reg)
# var              p value
#census_region2    .0983
#household_size6   .0144
#household_income6 .0480
#retail_index      .2732
#ecom_index        4.78e-05

# Redo knn with 
seed = with(cs,cbind(ecom_index,household_income==6,household_size==6))
targ = with(ts,cbind(ecom_index,household_income==6,household_size==6))

head(seed)
summary(seed)

head(targ)
summary(targ)

# Storage
profit4=rep(0,20)

# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit4[k] = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
}

# Plot
plot(profit4,type='b',pch=19)
title(main = "Exhibit 2: DMP Profits Significant Variables")
abline(h=2,lty=2)
# actually made much more money when we figured out the variables that matter

# Max profits
k.star = which.max(profit4); k.star #k = 3
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit4.star = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
profit4.star #4,413.61

##
mk.star$nn.index

# Explore matches
# How many did we really match?
table(target2[matches,"spend"]>0)
#TRUE: 79 FALSE:89

# What percentage did we capture?
perc.captured = sum(target2[matches,"spend"])/sum(target2[,"spend"])
perc.captured #75%


#---------------- 4/8/2017

# Evaluating if profits can be increased by changing the criteria for 'valuable' customer
# Also using only the 3 significant variables

summary(cust2[cust2$spend>0,"spend"])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.53   14.14   29.94   97.37   61.12 6768.00

v = c(1:150)
profit5.star = rep(0,150)
#perc.captured = rep(0,150)

#Looping different criteria for 'valuable' customer
for(i in 1:150){

cs=cust2[cust2$spend>v[i],]

seed = with(cs,cbind(ecom_index,household_income==6,household_size==6))
targ = with(target2,cbind(ecom_index,household_income==6,household_size==6))

profit5=rep(0,150)

# kNN Loop
  for(k in 1:40){
    mk = get.knnx(targ,seed,k=k,algorithm='brute')
    matches=unique(as.vector(mk$nn.index))
    profit5[k] = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
  }

# Max profits
k.star = which.max(profit5)
profit5.star[i] = profit5[k.star]

mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))

# What percentage did we capture?
#perc.captured[i] = sum(target2[matches,"spend"])/sum(target2[,"spend"])

}

profit5.star
perc.captured

spend.threshold = data.frame(v)
profit5.star = data.frame(profit5.star)
perc.captured = data.frame(perc.captured)
output = cbind(spend.threshold,profit5.star,perc.captured)
output

plot(profit5.star$profit5.star,type='b',pch=19,xlab = "$ spend cutoff", ylab = "profit")
title(main = "Exhibit 3: DMP Profits for Spend Cutoffs")

plot(profit5.star$profit5.star,type='b',pch=19,ylim = range(2000:5000),xlab = "$ spend cutoff", ylab = "profit")
title(main = "Exhibit 4:DMP and In House Profits for Spend Cutoffs")
lines(profit2a.star$profit2a.star,type='b')
abline(h=3559,lty=2)
abline(h=3615,lty=2)
abline(h=4626,lty=2)
abline(v=59, lty=2)
abline(v=43, lty=2)

max(profit5.star)
#4626.11
max(profit2a.star)
#3615.51

# v    profit3.star perc.captured
# 10      4413.61     0.7537596
# 14      4448.23     0.7965601
# 30      4518.86     0.7924764
# 35      4596.86     0.7924764
# 60      4581.26     0.7886237
# 100     4353.90     0.7807421

# Thus @ cost of $3.25, maximum profit can be achieved at threshold of spend > $35
# Max profit: $4,596.86 Percentage of spend captured in target file: 79.2%


# Evaluating the in-class example, which can be considered as the alternate option
# Cost = 0.5 and retail_index variable

cost = .5

load(file="C:/Booth/Digital & Algorithmic Marketing/Session 2/cust.rdat")
load(file="C:/Booth/Digital & Algorithmic Marketing/Session 2/target.rdat")

summary(cust[cust$spend>0,"spend"])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.53   14.14   29.94   97.37   61.12 6768.00

v = c(10,14,30,35,60,100)
profit3.star = rep(0,6)
perc.captured = rep(0,6)

#Looping different criteria for 'valuable' customer
for(i in 1:6){
  
  cs=cust[cust$spend>v[i],]
  
  seed = with(cs,cbind(retail_index,household_income==6,household_size==6))
  targ = with(target,cbind(retail_index,household_income==6,household_size==6))
  
  profit3=rep(0,20)
  
  # kNN Loop
  for(k in 1:20){
    mk = get.knnx(targ,seed,k=k,algorithm='brute')
    matches=unique(as.vector(mk$nn.index))
    profit3[k] = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])
  }
  
  # Max profits
  k.star = which.max(profit3)
  profit3.star[i] = profit3[k.star]
  
  mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
  matches=unique(as.vector(mk.star$nn.index))
  
  # What percentage did we capture?
  perc.captured[i] = sum(target[matches,"spend"])/sum(target[,"spend"])
  
}

profit3.star
perc.captured

spend.threshold = data.frame(v)
profit3.star = data.frame(profit3.star)
perc.captured = data.frame(perc.captured)
output = cbind(spend.threshold,profit3.star,perc.captured)
output

# v    profit3.star perc.captured
# 10      3559.14     0.5588503
# 14      3545.64     0.5588503
# 30      3566.51     0.5629340
# 35      3573.51     0.5629340
# 60      3615.51     0.5629340
# 100     3578.51     0.5629340

# The max. profit here is only $3,615.51, so the firm can make more profits by going with the costlier DMP
