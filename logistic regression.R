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

stock_model = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=training_data
                  ,family = binomial)
summary(stock_model)


###Use a fitted model to do predictions for test data

model_pred_probs = predict(stock_model,testing_data,type='response') #logistic regression gives probability and not class
model_pred_Direction = rep("Down",252)
model_pred_Direction[model_pred_probs>0.5] = "Up"


### Create confusion matrix to check accuracy of model and compute misclassification rate
table(model_pred_Direction, Direction_testing) ##non diagonals are misclassified
mean(model_pred_Direction != Direction_testing) #misclassification rate
