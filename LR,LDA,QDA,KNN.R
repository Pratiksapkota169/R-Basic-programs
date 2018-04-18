

#LR : 0.44
#LDA :0.44
#QDA : 0.40
#KNN : 0.46






### Loading file and finding correlations visually and statistically
library(ISLR)
attach(Smarket)

?Smarket
summary(Smarket)

cor(Smarket[,-9])
pairs(Smarket[,-9])

###Split the data into training and test dATA set

training = (Year<2005)  ##will contain true or false
testing = !training
training_data =Smarket[training,]
testing_data=Smarket[testing,]
Direction_testing = Direction[testing]

###Fit logistic model using training data

stock_model = glm(Direction~Lag1+Lag2, data=training_data,family = binomial)
summary(stock_model)
###Use a fitted model to do predictions for test data
model_pred_probs = predict(stock_model,testing_data,type='response') #logistic regression gives probability and not class
model_pred_Direction = rep("Down",252)
model_pred_Direction[model_pred_probs>0.5] = "Up"
### Create confusion matrix to check accuracy of model and compute misclassification rate
table(model_pred_Direction, Direction_testing) ##non diagonals are misclassified
mean(model_pred_Direction != Direction_testing) #misclassification rate





###LDA
## fit lda model on training data
lda_model = lda(Direction~Lag1+Lag2, data=training_data)
## fit Lda model on test data
lda_pred = predict(lda_model, testing_data)
names(lda_pred)

lda_pred_DirectionOfClass = lda_pred$class
##confusion matrix 
table(lda_pred_DirectionOfClass, Direction_testing)
mean(lda_pred_DirectionOfClass != Direction_testing) #misclassification rate


###QDA

qda_model = qda(Direction~Lag1+Lag2, data=training_data)
qda_pred = predict(qda_model, testing_data)
qda_pred_Direction = qda_pred$class
table(qda_pred_Direction, Direction_testing)
mean(qda_pred_Direction != Direction_testing) #misclassification rate

###KNN
library(class)
fix(Smarket)
std_data = scale(Smarket[,c(2,3)])
std_data
training_data=std_data[training,]
testing_data = std_data[testing,]
training_Direction = Direction[training]

knn_pred_Direction = knn(training_data,testing_data,training_Direction,5) #K=5

table(knn_pred_Direction,Direction_testing)
mean(knn_pred_Direction != Direction_testing)


