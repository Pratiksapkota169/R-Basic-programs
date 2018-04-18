################ Tree
library (tree)

install.packages("tree")
library(ISLR)
attach(Carseats)
fix(Carseats)
High=ifelse(Sales <=8,"No","Yes ")
Carseats =data.frame(Carseats ,High)                  # attach high column in the data frame
fix(Carseats)
tree.carseats =tree(High~.-Sales , Carseats )     #PLOT CARSEAT TREE
summary(tree.carseats )
plot(tree.carseats )
text(tree.carseats ,pretty =0)
tree.carseats




set.seed(2)
 train=sample (1: nrow(Carseats ), 200)
 Carseats.test=Carseats [-train ,]                                        # TEST TREE 
 High.test=High[-train]
 tree.carseats =tree(High~.-Sales , Carseats ,subset=train)
 tree.pred=predict(tree.carseats ,Carseats.test ,type="class")
 table(tree.pred ,High.test)
(86+57)/200 

 
 set.seed(3)
 cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )  #apply cross validation for pruning
 names(cv.carseats )
 cv.carseats
 
 par(mfrow=c(1,2))
 plot(cv.carseats$size ,cv.carseats$dev ,type="b") 
 plot(cv.carseats$k ,cv.carseats$dev ,type="b") 
 prune.carseats=prune.misclass(tree.carseats,best=9)                   #PRUNE WITH TERMINAL =9
 plot(prune.carseats )                                                     #$dev is cross validation error value
 text(prune.carseats ,pretty =0) 

 
 tree.pred=predict(prune.carseats ,Carseats.test , type="class")
table(tree.pred ,High.test)
(94+60) /200



prune.carseats =prune.misclass (tree.carseats ,best=15)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
tree.pred=predict(prune.carseats ,Carseats.test , type="class")    # pRUNE WITH TERMINSAL=15
table(tree.pred ,High.test)
(86+62) /200




############Fitting Regression trees
library(MASS)
attach(Boston)
fix(Boston)

library(tree)
library(MASS)
attach(Carseats)
set.seed(1)
train = sample (1: nrow(Carseats), nrow(Carseats)/2)
train.model = tree(Sales~.,Carseats,subset=train)
summary(train.model)
par(mfrow=c(1,2))
plot(train.model)
text(train.model ,pretty =0)

cv.carseats = cv.tree(train.model)
plot(cv.carseats$size, cv.carseats$dev,type="b")
which.min(cv.carseats$size)


prune.carseats =prune.tree(train.model ,best =5)
(prune.carseats)
text(prune.carseats ,pretty =0)
############Bagging and Random Forest


library (randomForest)

set.seed (1)
Carseats.test = Carseats[-train,]

High=ifelse(Sales <=8,"No","Yes ")
Carseats =data.frame(Carseats ,High)
bag.train =randomForest(High~.-Sales,data=Carseats ,subset =train ,
                           mtry=10, importance =TRUE)
bag.train
bag.pred = predict (bag.train ,newdata =Carseats[-train ,])
plot(bag.pred , Carseats.test)
abline (0,1)
mean(( bag.pred-Carseats.test)^2)




#tree =25

bag.train =randomForest(High~.-Sales,data=Carseats ,subset =train ,
                        mtry=13, importance =TRUE, ntree=25)
bag.train
bag.pred = predict (bag.train ,newdata =Carseats[-train ,])
plot(bag.pred , Carseats.test)
abline (0,1)
mean(( bag.pred-carseats.test)^2)

set.seed (1)
rf.train =randomForest(High~.-Sales,data=Carseats ,subset =train ,
                          mtry=6, importance =TRUE)
rf.pred = predict (rf.train ,newdata =Carseats[-train ,])
mean(( rf.pred -carseats.test)^2)
importance (rf.pred)
varImpPlot (rf.pred)


#########Boosting



library (gbm)
set.seed (1)
boost.boston =gbm(High~.-Sales,data=Boston [train,], distribution="gaussian ",n.trees =5000 , interaction.depth=4)
summary (boost.boston )
par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i=" lstat ")
yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],
                    n.trees =5000)
mean(( yhat.boost -boston.test)^2)
boost.boston =gbm(High~.-Sales,data=Boston [train ,], distribution=
                    "gaussian ",n.trees =5000 , interaction .depth =4, shrinkage =0.2,
                  verbose =F)
yhat.boost=predict (boost .boston ,newdata =Boston [-train ,],
                      n.trees =5000)
mean(( yhat.boost -boston .test)^2)











