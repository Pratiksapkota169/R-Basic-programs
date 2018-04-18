credit<-read.csv("Credit_Dataset.csv")
fix(credit)


credit$PROFITABLE <- ifelse(credit$PROFIT > 0,1,0)
fix(credit)


credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$SAV_ACCT <- as.factor(credit$SAV_ACCT)
credit$HISTORY <- as.factor(credit$HISTORY)
credit$JOB <- as.factor(credit$JOB)
credit$TYPE <- as.factor(credit$TYPE)

set.seed(11217)
#Partition Data set by 30%-70% for test and remaining data
ran_testing_data = sample(nrow(credit), 0.3*nrow(credit))
credit_testing_data <- credit[ran_testing_data,]

credit_remaining_data <- credit[-ran_testing_data,]

remaining_valid_data = sample(nrow(credit_remaining_data), 0.25*nrow(credit_remaining_data))
credit_remaining_valid_data = credit_remaining_data[remaining_valid_data,]
credit_remaining_training_data = credit_remaining_data[-remaining_valid_data,]




log_model=glm(PROFITABLE~ AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE,data=credit_remaining_training_data, family = binomial)
summary(log_model)

log_preds <- predict(log_model,newdata=credit_remaining_valid_data,type="response")
log_class <- ifelse(log_preds>.7,1,0)

table(credit_remaining_valid_data$PROFITABLE,log_class)

accuracy_validation = sum(ifelse(credit_remaining_valid_data$PROFITABLE==log_class,1,0))/nrow(credit_remaining_valid_data)
accuracy_validation

library(ROCR)
library(gplots)
pred<-prediction(log_preds,credit_remaining_valid_data$PROFITABLE)
par(mfrow=c(3,1))

perf.acc<-performance(pred,"acc")
plot(perf.acc)


perf.sens<-performance(pred,"sens")
plot(perf.sens)

perf.spec<-performance(pred,"spec")
plot(perf.spec)

roc_val <-performance(pred,"tpr","fpr")

log_pred_train<-predict(log_model,newdata=credit_remaining_training_data, type="response")
pred_train<-prediction(log_pred_train,credit_remaining_training_data$PROFITABLE)
roc_train<-performance(pred_train,"tpr","fpr")
plot(roc_val,type='l',col="black")
plot(roc_train,type='l',col="green",add="TRUE")





###Trees

library(tree)
credit.tree=tree(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE,credit_remaining_training_data)
summary(credit.tree)
plot(credit.tree)
text(credit.tree)

plot(credit.tree)
text(credit.tree,pretty=1)


tree_preds1 <- predict(credit.tree,newdata=credit_remaining_valid_data)
pred1 <- prediction(tree_preds1,credit_remaining_valid_data$PROFITABLE)
acc1.perf = performance(pred1, measure="acc")
acc1.perf
plot(acc1.perf)
points (col="red",cex=2,pch =20)




####Pruned Tree=1
credit.pruned1=prune.tree(credit.tree,best=1)
summary(credit.pruned1)
plot(credit.pruned1)
text(credit.pruned1,pretty=1)

tree_preds.pruned1 <- predict(credit.pruned1,newdata=credit_remaining_valid_data)
pred.pruned1 <- prediction(tree_preds.pruned1,credit_remaining_valid_data$PROFITABLE)
acc1.perf1 = performance(pred.pruned1, measure="acc")
acc1.perf1
plot(acc1.perf1)




########Pruned Tree =2
credit.pruned2=prune.tree(credit.tree,best=2)
summary(credit.pruned2)
plot(credit.pruned2)
text(credit.pruned2,pretty=1)

tree_preds.pruned2 <- predict(credit.pruned2,newdata=credit_remaining_valid_data)
pred.pruned2 <- prediction(tree_preds.pruned2,credit_remaining_valid_data$PROFITABLE)
acc1.perf2 = performance(pred.pruned2, measure="acc")
acc1.perf2
plot(acc1.perf2)

best2 = which.max(slot(acc1.perf2,"y.values")[[1]])
max.acc2 = slot(acc1.perf2,"y.values")[[1]][best2]
max.cutoff2 = slot(acc1.perf2,"x.values")[[1]][best2]
print(c(accuracy= max.acc2, cutoff = max.cutoff2))

####Pruned Tree=5
credit.pruned5=prune.tree(credit.tree,best=5)
summary(credit.pruned5)
plot(credit.pruned5)
text(credit.pruned5,pretty=1)

tree_preds.pruned5 <- predict(credit.pruned5,newdata=credit_remaining_valid_data)
pred.pruned5 <- prediction(tree_preds.pruned5,credit_remaining_valid_data$PROFITABLE)
acc1.perf5 = performance(pred.pruned5, measure="acc")
acc1.perf5
plot(acc1.perf5)

best5 = which.max(slot(acc1.perf5,"y.values")[[1]])
max.acc5 = slot(acc1.perf5,"y.values")[[1]][best5]
max.cutoff5 = slot(acc1.perf5,"x.values")[[1]][best5]
print(c(accuracy= max.acc5, cutoff = max.cutoff5))


######Pruned Tree=10
credit.pruned10=prune.tree(credit.tree,best=10)
summary(credit.pruned10)
plot(credit.pruned10)
text(credit.pruned10,pretty=1)



tree_preds.pruned10 <- predict(credit.pruned10,newdata=credit_remaining_valid_data)
pred.pruned10 <- prediction(tree_preds.pruned10,credit_remaining_valid_data$PROFITABLE)
acc1.perf10 = performance(pred.pruned10, measure="acc")
acc1.perf10
plot(acc1.perf10)

best10 = which.max(slot(acc1.perf10,"y.values")[[1]])
max.acc10 = slot(acc1.perf10,"y.values")[[1]][best10]
max.cutoff10 = slot(acc1.perf10,"x.values")[[1]][best10]
print(c(accuracy= max.acc10, cutoff = max.cutoff10))


#####Pruned Tree=15
credit.pruned15=prune.tree(credit.tree,best=15)
summary(credit.pruned15)
plot(credit.pruned15)
text(credit.pruned15,pretty=1)



tree_preds.pruned15 <- predict(credit.pruned15,newdata=credit_remaining_valid_data)
pred.pruned15 <- prediction(tree_preds.pruned15,credit_remaining_valid_data$PROFITABLE)
acc1.perf15 = performance(pred.pruned15, measure="acc")
acc1.perf15
plot(acc1.perf15)

best15 = which.max(slot(acc1.perf15,"y.values")[[1]])
max.acc15 = slot(acc1.perf15,"y.values")[[1]][best15]
max.cutoff15 = slot(acc1.perf15,"x.values")[[1]][best15]
print(c(accuracy= max.acc15, cutoff = max.cutoff15))

#####################KNN









## For kNN, use the class library
library(class)


## Our tree variables were the 2,3,4,5, and 8 columns
train.X=credit_remaining_training_data[,c(2,3,4,6,7,10,12,17,18,19)]
valid.X=credit_remaining_valid_data[,c(2,3,4,6,7,10,12,17,18,19)]
test.X=credit_testing_data[,c(2,3,4,6,7,10,12,17,18,19)]

train.profit=as.numeric(credit_remaining_training_data$PROFITABLE)
valid.profit=as.numeric(credit_remaining_valid_data$PROFITABLE)
test.profit=as.numeric(credit_testing_data$PROFITABLE)




knn.pred1=knn(train.X,valid.X,train.profit,k=1)

table(valid.profit,knn.pred1)

accuracy1 = sum(ifelse(valid.profit==knn.pred1,1,0))/175
accuracy1



knn.pred3=knn(train.X,valid.X,train.profit,k=3)

table(valid.profit,knn.pred3)


accuracy3 = sum(ifelse(valid.profit==knn.pred3,1,0))/175
accuracy3

knn.pred5=knn(train.X,valid.X,train.profit,k=5)

table(valid.profit,knn.pred5)
accuracy5 = sum(ifelse(valid.profit==knn.pred5,1,0))/175
accuracy5


knn.pred10=knn(train.X,valid.X,train.profit,k=10)
table(valid.profit,knn.pred10)
accuracy10 = sum(ifelse(valid.profit==knn.pred10,1,0))/175
accuracy10

knn.pred25=knn(train.X,valid.X,train.profit,k=25)

table(valid.profit,knn.pred25)
accuracy25 = sum(ifelse(valid.profit==knn.pred25,1,0))/175
accuracy25


#########
library(rpart)
credit.tree1=rpart(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE,credit_remaining_training_data)
plotcp(credit.tree1)
rsq.rpart(credit.tree1)
