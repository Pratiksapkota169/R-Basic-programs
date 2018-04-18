credit<-read.csv("Credit_Dataset.csv")
fix(credit)
set.seed(11217)

#credit <- credit[,-1]    To remove OBS
#fix(credit)


##Partition 30% of the data for testing data and 70% of the data for training data 
testing_data = sample(nrow(credit), 0.3*nrow(credit))
credit_testing_data <- credit[testing_data,]
credit_train_data <- credit[testing_data,]

credit_all <- lm(PROFIT~.-OBS, data=credit_train_data)
summary(credit_all)
credit_null <- lm(PROFIT~1, data=credit_train_data)
#Forward Selection
forward_model = step(credit_null, scope=list(upper=credit_all), direction="forward")
summary(forward_model)
forward_model_pred= predict(forward_model, data=credit_testing_data)
sqrt(mean((credit_testing_data$PROFIT-forward_model_pred)^2))


#Backward Elimination
backward_model = step(credit_all, scope=list(upper=credit_null),direction="backward")
summary(backward_model)
backward_model_pred= predict(backward_model, data=credit_testing_data)
sqrt(mean((credit_testing_data$PROFIT-backward_model_pred)^2))

#A stepwise procedure starting with forward selection
forward_model_both = step(credit_null, scope=list(upper=credit_all), direction="both")
summary(forward_model_both)
forward_model_both_pred= predict(forward_model_both, data=credit_testing_data)
sqrt(mean((credit_testing_data$PROFIT-forward_model_both_pred)^2))

#A stepwise procedure starting with backward elimination
backward_model_both = step(credit_all, scope=list(upper=credit_null), direction="both")
summary(backward_model_both)
backward_model_both_pred= predict(backward_model_both, data=credit_testing_data)
sqrt(mean((credit_testing_data$PROFIT-backward_model_both_pred)^2))

library(leaps)

bss=regsubsets (PROFIT~.,data=credit[,-1] ,nvmax=21)
summary(bss)
coef(bss, 12)

library(glmnet)
set.seed(11217)
#Ridge Regression
ridge = glmnet(as.matrix(credit_testing_data[,c(2:22)]),credit_testing_data$PROFIT,alpha=0)
ridge.cv=cv.glmnet(as.matrix(credit_testing_data[,c(2:22)]),credit_testing_data$PROFIT,alpha=0)
plot(ridge.cv)
best.lambda=ridge.cv$lambda.min
best.lambda

ridge.pred = predict(ridge,s=best.lambda,newx=as.matrix(credit_testing_data[,c(2:22)]))
sqrt(mean((credit_testing_data$PROFIT-ridge.pred)^2))
predict(ridge,s=best.lambda,type="coefficients")


#Lasso
set.seed(11217)
lasso = glmnet(as.matrix(credit_testing_data[,c(2:22)]),credit_testing_data$PROFIT,alpha=1)

lasso.cv=cv.glmnet(as.matrix(credit_testing_data[,c(2:22)]),credit_testing_data$PROFIT,alpha=1)
plot(lasso.cv)


best.lambda=lasso.cv$lambda.min
best.lambda

lasso.pred = predict(lasso,s=best.lambda,newx=as.matrix(credit_testing_data[,c(2:22)]))

sqrt(mean((credit_testing_data$PROFIT-lasso.pred)^2))
predict(lasso,s=best.lambda,type="coefficients")

