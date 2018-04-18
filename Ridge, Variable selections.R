######## CHapter No 6#####################

###########Best Subset Selection#############
library(ISLR)
attach(Hitters)
fix(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))                             ## detect records having blank cells
Hitters = na.omit(Hitters)                             ##delete records having blank cells
fix(Hitters)
sum(is.na(Hitters))

library(leaps)                                        #regsubsets used leap library
bss= regsubsets(Salary~., Hitters,nvmax=19)
summary(bss)
names(summary(bss))

####Rsq#####
summary(bss)$rsq
par(mfrow = c(2,2))
plot(summary(bss)$rsq,xlab="Number of variables", ylab="Rsquare", type = "l")  

####Rss########

plot(summary(bss)$rss,xlab="Number of variables", ylab="RSS", type = "l")

#####adjr2###########

plot(summary(bss)$adjr2,xlab="Number of variables", ylab="adjR2", type = "l")
which.max(summary(bss)$adjr2)
points(11,summary(bss)$adjr2[11],col="red",cex=2,pch=20)


############Cp##############

plot(summary(bss)$cp,xlab="Number of variables", ylab="cp", type = "l")
which.min(summary(bss)$cp)
points(10,summary(bss)$cp[10],col="red",cex=2,pch=20)

########bic###############

plot(summary(bss)$bic,xlab="Number of variables", ylab="bic", type = "l")
which.min(summary(bss)$bic)
points(6,summary(bss)$bic[6],col="red",cex=2,pch=20)
coef(bss,7)

###############forward subset selection################

fwd = regsubsets(Salary~., Hitters, nvmax=19, method ="forward")
coef(fwd,7)

########backward subset selection###############
bwd = regsubsets(Salary~., Hitters, nvmax=19, method ="backward")
coef(bwd,7)

#################Use vaiidation and cross validation for variable selections########
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters),rep= TRUE )
test = (!train)

bss = regsubsets(Salary~., Hitters[train,],nvmax = 19)
test.mat=model.matrix(Salary~.,Hitters[test,])
Hitters$Salary[test]
val.errors = rep(NA,19)
for(i in 1:19)
{
  coefi = coef(bss,id= i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(bss,10)
bss = regsubsets(Salary~.,Hitters,nvmax=19)
coef(bss,10)


###################k/10 fold cross validation for variable selection##############

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
error=matrix(NA,k,19,dimnames =list(NULL,paste(1:19)))

predict.regsubsets=function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
  }

for(j in 1:k){
  bss=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
   for(i in 1:19){
   pred=predict(bss,Hitters[folds ==j,],id=i)
   error[j,i]= mean((Hitters$Salary[folds==j]-pred)^2)
 }
}
mean.error = apply(error,2,mean)
mean.error
par(mfrow=c(1,1))
plot(mean.error,type = 'b')
which.min(mean.error)
bss = regsubsets(Salary~.,Hitters,nvmax=19)
coef(bss,11)


##########Ridge Regression###########33


library(glmnet)
x= model.matrix(Salary~.-1,Hitters)
y= Hitters$Salary
ridge = glmnet(x,y,alpha=0)
plot(ridge, xvar = 'lambda',label = TRUE) 
dim(coef(ridge))
cv.ridge = cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

