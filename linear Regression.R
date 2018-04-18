##Simple Linear Regression in R

library(MASS)
attach(Boston)
names(Boston)

lm.fit=lm(medv~lstat)
summary(lm.fit)
plot(lm.fit)
plot(lstat,medv,main = "Scatterplot",xlab="Lstat",ylab="Median Value")
abline(lm.fit,col="red",lwd=6)

##Multiple Linear Regression
#Scatter plot Matrix (vvImp)
pairs(Boston)   #Entire Scatter plot matrix
pairs(Boston[,c(1,3,7)]) #Scatter plot matrix of 3 variables
lm.fit2=lm(medv~lstat+age) #include 2 variables in model
summary(lm.fit2)

lm.fit3=lm(medv~., data=Boston) #include all coefficients in regression model
plot(fit3)

summary(lm.fit3)


lm.fit4=lm(medv~.-age, data=Boston) # All coefficients without age
summary(lm.fit4)

##interaction Terms
lm.fit5=lm(medv~lstat*age)
summary(lm.fit5)

lm.fit6=lm(medv~lstat+lstat:age)
summary(lm.fit6)

##Non-linear Transformation of cariables
lm.fit7=lm(medv~poly(lstat,2))
summary(lm.fit7)
lm.fit7=lm(medv~poly(lstat,6))
summary(lm.fit7)

