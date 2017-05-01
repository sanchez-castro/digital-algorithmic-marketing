# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Homework #4 (Group)

# The persado_experiment.xlsx file contains a real experiment run by Persado
# Use the data to answer the following questions.

# Q1: Assuming that all relevant variables were tested how many possible
#		message combinations are there? 
# Q2: Estimate two logit models based on the data (assuming a simple linear 
#		specification) - one for opens and another for clicks. Discuss your results based
#		on the nature of the variables (see variable names and descriptions tab).
# Q3: Use the estimated models to compute predicted probabilities for all possible message
# 		combinations (separate predictions for opens and clicks). 
#		Which messages have the highest fitted response probabilities (for opens and clicks each)?
#		Are the two messages similar or different? Discuss.

setwd("C:/Booth/Digital & Algorithmic Marketing/HW4")

library(data.table)
library(AlgDesign)

#data manipulations done to the original persado_experiment data:
#deleted the Control row
#Converted L1, L2, L3, L4 to 1, 2, 3, 4 respectively
pers = fread("persado_experiment_v2.csv", header=TRUE,
              stringsAsFactors = TRUE)
summary(pers)
head(pers)

# cols = c("intro","headline","main_text","button","action","purpose","symbol")
# pers[cols] = lapply(pers[cols],factor)

pers$intro = as.factor(pers$intro)
pers$headline = as.factor(pers$headline)
pers$main_text = as.factor(pers$main_text)
pers$button = as.factor(pers$button)
pers$action = as.factor(pers$action)
pers$purpose = as.factor(pers$purpose)
pers$symbol = as.factor(pers$symbol)
summary(pers)

pers$unique_not_opened = pers$unique_sent - pers$bounced - pers$unique_opened
pers$unique_not_clicked = pers$unique_opened - pers$unique_clicks
head(pers)

#Q2

ds.hw = pers[,9:15]
head(ds.hw)
Xm.hw = model.matrix(~.,ds.hw)
head(Xm.hw)

# logit model for opens
#y.opens is successes (opens) and total trials (not opened)
y.opens = pers[,c(7,16)]
head(y.opens)
class(y.opens) #data.frame
y.opens = data.matrix(y.opens)
logit.opens = glm(y.opens~Xm.hw-1,family='binomial')

summary(logit.opens)
#                    Estimate Std. Error z value   Pr(>|z|)    
# Xm.hw(Intercept)  0.2169715  0.0075458  28.754  < 2e-16 ***
# Xm.hwintro2       0.0126610  0.0057235   2.212 0.026961 *  
# Xm.hwintro3       0.0176689  0.0057210   3.088 0.002012 ** 
# Xm.hwintro4       0.0088201  0.0057270   1.540 0.123539    
# Xm.hwheadline2    0.0106398  0.0057209   1.860 0.062912 .  
# Xm.hwheadline3    0.0057729  0.0057250   1.008 0.313276    
# Xm.hwheadline4    0.0083734  0.0057283   1.462 0.143807    
# Xm.hwmain_text2  -0.0089756  0.0040491  -2.217 0.026643 *  
# Xm.hwbutton2      0.0009709  0.0040491   0.240 0.810505    
# Xm.hwaction2     -0.0193596  0.0040491  -4.781 1.74e-06 ***
# Xm.hwpurpose2     0.0063919  0.0057344   1.115 0.265001    
# Xm.hwpurpose3    -0.0048107  0.0057286  -0.840 0.401035    
# Xm.hwpurpose4    -0.0004229  0.0057234  -0.074 0.941097    
# Xm.hwsymbol2      0.0150448  0.0040491   3.716 0.000203 ***

# logit model for clicks
#y.opens is successes (clicks) and total trials (opened, but not clicked)
y.clicks = pers[,c(8,17)]
head(y.clicks)
class(y.clicks) #data.frame
y.clicks = data.matrix(y.clicks)
logit.clicks = glm(y.clicks~Xm.hw-1,family='binomial')

summary(logit.clicks)
#                    Estimate Std. Error  z value  Pr(>|z|)    
# Xm.hw(Intercept) -1.575363   0.013302 -118.428  < 2e-16 ***
# Xm.hwintro2      -0.004588   0.009936   -0.462  0.64427    
# Xm.hwintro3       0.124729   0.009722   12.829  < 2e-16 ***
# Xm.hwintro4      -0.010023   0.009955   -1.007  0.31401    
# Xm.hwheadline2    0.102561   0.009857   10.405  < 2e-16 ***
# Xm.hwheadline3    0.085012   0.009896    8.591  < 2e-16 ***
# Xm.hwheadline4    0.045417   0.009963    4.558 5.15e-06 ***
# Xm.hwmain_text2  -0.007379   0.006960   -1.060  0.28903    
# Xm.hwbutton2     -0.011281   0.006961   -1.621  0.10507    
# Xm.hwaction2      0.160196   0.006959   23.020  < 2e-16 ***
# Xm.hwpurpose2    -0.028137   0.009702   -2.900  0.00373 ** 
# Xm.hwpurpose3    -0.103909   0.009838  -10.562  < 2e-16 ***
# Xm.hwpurpose4    -0.102917   0.009818  -10.482  < 2e-16 ***
# Xm.hwsymbol2     -0.013481   0.006963   -1.936  0.05285 .    


#Q3

intro = c("Thank you! Enjoy MORE Everything ", "Welcome to MORE Everything ",
          "MORE Everything has been activated ", "MORE Everything is designed for you ")
headline = c("YOU'LL LOVE IT! ","CONGRATS! ","YOU'RE IN! ","FINALLY! ")
main_text = c("Make the most of your new plan's savings & shareable data - add a new device today! ",
              "Check out our selection for you - add a new phone, tablet, or other device! ")
button = c("--Symbol After text-- ","--Symbol Before text-- ")
action = c("--Uses word click-- ","--Does not use word click-- ")
purpose = c("Take A look","Have a Look","View More","See for Yourself")
symbol = c("???",">>")

# intro = c("L1","L2","L3","L4","N/A")
# headline = c("L1","L2","L3","L4","N/A")
# main_text = c("L1","L2","N/A")
# button = c("L1","L2","N/A")
# action = c("L1","L2","N/A")
# purpose = c("L1","L2","L3","L4","N/A")
# symbol = c("L1","L2","N/A")

Vs.hw = list(intro, headline, main_text,button,action,purpose,symbol)
levs.hw=sapply(Vs.hw,length)
prod(levs.hw) #1,024

mat2 = gen.factorial(levels=levs.hw,
                     varNames=c("intro", "headline", "main_text", "button", "action",
                                "purpose", "symbol"),  factors="all")
head(mat2)
tail(mat2)


# Scoring the models on all possible combinations
#opens model
newdat.hw = data.frame(Xm.full=I(model.matrix(~.,data=mat2)))
head(newdat.hw)
head(Xm.hw)
class(newdat.hw)
score.opens = predict(logit.opens,newdata=newdat.hw)
# Warning message:
#   'newdata' had 1024 rows but variables found have 16 rows 

predprob.opens = predict(logit.opens,newdata=newdat.hw, type='response')
head(predprob.opens)

#clicks model
score.clicks = predict(logit.clicks,newdata=newdat.hw)
predprob.clicks = predict(logit.clicks,newdata=newdat.hw, type='response')
head(predprob.clicks)

# What's the message
#Opens
idx.opens = which.max(score.opens)
idx.opens  # 12
opt.mess.opens = as.numeric(mat2[idx.opens,])
opt.mess.opens #4 3 1 1 1 1 1
predprob.opens[12]
# 0.56573

# Example
message.opens = ""
for(j in 1:7){
  message=paste(message,c(Vs.hw[[j]])[opt.mess.opens[j]],sep="")
}

message.opens

"MORE Everything is designed for you YOU'RE IN! 
Make the most of your new plan's savings & shareable data - add a new device today! 
#Symbol After text# #Uses word click# Take A look ???"

#Clicks
idx.clicks = which.max(score.clicks)
idx.clicks  # 11
opt.mess.clicks = as.numeric(mat2[idx.clicks,])
opt.mess.clicks #3 3 1 1 1 1 1
predprob.clicks[11]
# 0.2113655

# Example
message.clicks = ""
for(j in 1:7){
  message=paste(message,c(Vs.hw[[j]])[opt.mess.clicks[j]],sep="")
}

message.clicks

"MORE Everything has been activated YOU'RE IN! 
Make the most of your new plan's savings & shareable data - add a new device today! 
#Symbol After text# #Uses word click# Take A look ???"
