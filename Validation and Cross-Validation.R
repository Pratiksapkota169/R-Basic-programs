library(ISLR)
library(boot)
set.seed(1)
attach(Auto)
fix(Auto)
###LOOCV##########                                   ->cv.glm(dataset,model)
model = glm(mpg~horsepower, data = Auto)
model$residuals
model$linear.predictors
summary(model)
loocv = cv.glm(Auto, model)
loocv$delta[1]

###For polynomials 1-5####
mse_loocv = NULL
for(i in 1:5)
{
  model = glm(mpg~poly(horsepower,i), data=Auto)
  mse_loocv[i] = cv.glm(Auto, model)$delta[1]
}
mse_loocv



############k_fold_cross_validation###############      ->cv.glm(dataset,model,k)

model = glm(mpg~horsepower, data = Auto)             #linear model
summary(model)
k_fold_10 = cv.glm(Auto, model,K=10)
k_fold_10$delta[1]

###For polynomials 1-5####
mse_k_fold_10 = NULL
for(i in 1:5)
{
  model = glm(mpg~poly(horsepower,i), data=Auto)
  mse_k_fold_10[i] = cv.glm(Auto, model, K=10)$delta[1]
}
mse_k_fold_10
plot(1:5,mse_loocv, type ="b")
lines(1:5,mse_k_fold_10, type ="b",col="red")

#########################################Validation set approach###########################
set.seed(1)                                  # for random seed 1
train=sample (392,196)                       # 196 random data used for training set
poly1=glm(mpg~horsepower ,data=Auto ,subset=train)    #linear regression 


mean((mpg-predict (poly1 ,Auto))[-train]^2)            #test MSE for linear regression  #validation data
poly2=glm(mpg~poly(horsepower ,2),data=Auto , subset=train) # quadratic regression

predict(poly2,Auto)
poly2$linear.predictors
mean((mpg-predict (poly2 ,Auto ))[- train]^2)          #test MSE for quadratic linear regression #validation data
poly3=lm(mpg~poly(horsepower ,3),data=Auto , subset=train)  # cubic regression
mean((mpg-predict (poly3 ,Auto ))[- train]^2)          #test MSE for cubic linear regression #validation data

######################### Bootstrap #############################

#Create a function to calculate alpha
fix(Portfolio)
alpha.fn=function (data ,index){
  X=data$X[index]      
  Y=data$Y[index]      
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
}

set.seed(1)
alpha.fn(Portfolio ,1:100)   #function call with 100 observations  #will remain same everytime

alpha.fn(Portfolio ,sample (100,100, replace=T))  #claculate aplha with replacement of observations  #this changes everytime 

boot(Portfolio ,alpha.fn,R=1000)  #1000 simulated bootstraps

###############Bootstrap assessing accuracy of linear regression model #########################33

boot.fn=function (data ,index) {
  return(coef(lm(mpg~horsepower ,data=data , subset=index)))    #returns coefficient B0 and B1(intercept and slope)
}
boot.fn(Auto ,1:392)

set.seed(1)
boot.fn(Auto ,sample (392,392, replace=T))
boot.fn(Auto ,sample (392,392, replace=T))  #different from above


boot(Auto ,boot.fn ,R=1000)

summary (lm(mpg~horsepower ,data=Auto))$coef   #Bootstrap cooefficients are accurate than standard summary when
# there is non linearity

boot.fn=function (data ,index){
  coefficients(lm(mpg~horsepower +I(horsepower ^2),data=data ,   #quadratic regression comparison
                  subset=index))
}
set.seed(1)
boot.fn(Auto,1:392)
boot(Auto ,boot.fn ,R=1000)

summary (lm(mpg~horsepower +I(horsepower ^2),data=Auto))$coef


