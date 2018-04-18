library(ISLR)
#########################################Validation set approach###########################

set.seed(1)                                  # for random seed 1
train=sample (392,196)                       # 196 random data used for training set
lm.fit=lm(mpg~horsepower ,data=Auto ,subset=train)    #linear regression 
attach(Auto)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)            #test MSE for linear regression  #validation data
lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto , subset=train) # quadratic regression
mean((mpg -predict (lm.fit2 ,Auto ))[- train]^2)          #test MSE for quadratic linear regression #validation data
lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto , subset=train)  # cubic regression
mean((mpg -predict (lm.fit3 ,Auto ))[- train]^2)          #test MSE for cubic linear regression #validation data


set.seed(2)                                      #for random seed 2.......different random training set this time
train=sample (392,196)                           # 196 random data used for training set
lm.fit=lm(mpg~horsepower ,subset=train)                
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)                # different test MSE now

lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto , subset=train)
mean((mpg -predict (lm.fit2 ,Auto ))[- train]^2)               # different test MSE now

lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto , subset=train)
mean((mpg -predict (lm.fit3 ,Auto ))[- train]^2)              # different test MSE now



############################Leave one out cross validation################################


glm.fit=glm(mpg~horsepower ,data=Auto)     #we use glm because we to use cv.error function. glm works as 
                                          #linear regression if Family=Binomial is not stated else
                                           #it would work as logistic regression


coef(glm.fit)
library(boot)                          #cv.glm() is in boot library
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err=cv.glm(Auto ,glm.fit)
cv.err$delta                         #test MSE for linear regression training as well as validation
cv.error=rep(0,5)                       #Higher polynomials 
for (i in 1:5){
   glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
   cv.error[i]=cv.glm(Auto ,glm.fit)$delta [1]
   }
cv.error

############################ K or 10 fold cross validation ################################


set.seed(17)                         #random seed 3
cv.error.k.10=rep(0 ,10)             #Array of 10 initialized with value 0
for (i in 1:10){                      #Higher polynomials 1-10
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.k.10[i]=cv.glm(Auto ,glm.fit ,K=10) $delta [1]    # k=10 for 10 fold cross validation
   }
cv.error.k.10


######################### Bootstrap #############################

#Create a function to calculate alpha

alpha.fn=function (data ,index){
  X=data$X[index]      
  Y=data$Y[index]      
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
}

alpha.fn(Portfolio ,1:100)   #function call with 100 observations  #will remain same everytime

set.seed(1)
alpha.fn(Portfolio ,sample (100,100, replace=T))  #claculate aplha with replacement of observations  #this changes everytime 

boot(Portfolio ,alpha.fn,R=1000)  #1000 simulated bootstraps


###############Bootstrap assessing accuracy of linear regression model #########################33

boot.fn=function (data ,index)
  return(coef(lm(mpg~horsepower ,data=data , subset=index)))    #returns coefficient B0 and B1(intercept and slope)
boot.fn(Auto ,1:392)

set.seed(1)
boot.fn(Auto ,sample (392,392, replace=T))
boot.fn(Auto ,sample (392,392, replace=T))  #different from above

boot(Auto ,boot.fn ,R=1000)

summary (lm(mpg~horsepower ,data=Auto))$coef   #Bootstrap cooefficients are accurate than standard summary when
                                                 # there is non linearity

boot.fn=function (data ,index)
   coefficients(lm(mpg~horsepower +I(horsepower ^2),data=data ,   #quadratic regression comparison
                    subset=index))
set.seed(1)
boot(Auto ,boot.fn ,R=1000)

summary (lm(mpg~horsepower +I(horsepower ^2),data=Auto))$coef
















