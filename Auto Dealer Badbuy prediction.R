car <- read.csv("car_data.csv")  #read the car_data dataset in R
fix(car)                          #display data set
names(car)                       #variables used in dataset
set.seed(71923)
train_instn = sample(nrow(car), 0.7*nrow(car))  

car_train <- car[train_instn,]          #70% train data
car_test <- car[-train_instn,]          #30% test data

detach(car_train)
attach(car_train)
#boxplots
boxplot(VehOdo~IsBadBuy, ylab="vehOdo", xlab= "isBadBuy")
boxplot(VehicleAge~IsBadBuy, ylab="vehAge", xlab= "isBadBuy")

## two-way tables
make_table <- table(Make,IsBadBuy)
make_table

#lin_model1 <- lm(IsBadBuy ~., data=car_train)
#summary(lin_model1)

#Linear model
lin_model <- lm(IsBadBuy~ Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data = car_train)
summary(lin_model)

lin_train_preds <- predict(lin_model,newdata=car_train)




#difference <- lin_train_preds-car_train$IsBadBuy
#avgerror <- mean(difference)
#avgerror
#rmse<-sqrt(mean(difference^2))
#rmse

lin_test_preds <- predict(lin_model,newdata=car_test)
#difference1 <- lin_test_preds-car_test$IsBadBuy
#avgerror1 <- mean(difference1)
#avgerror1
#rmse1<-sqrt(mean(difference1^2))
#rmse1



##make a function to evaluate the performance using standard metrics
measure_perf <- function(predicted,actual){
  AE = mean(predicted-actual)
  RMSE = sqrt(mean((predicted-actual)^2))
  return(c(AE,RMSE))
}

##compute AE and RMSE on the training and test sets.
##which is better?
measure_perf(lin_train_preds, car_train$IsBadBuy)
measure_perf(lin_test_preds, car_test$IsBadBuy)


##actually classify the instances, using a cutoff of .5
class_train <- ifelse(lin_train_preds>.5,1,0)
class_test <- ifelse(lin_test_preds>.5,1,0)

##this is called a confusion matrix
##we'll cover more next week!
table(car_test$IsBadBuy, class_test)

#lin_test=rep(0,3019)
#lin_test[lin_test_preds>.5]=1
#table(car_test$IsBadBuy,lin_test)


##compute the predictive accuracy on the test data
##this is the number of correct classifications/the total number of instances

accuracy = sum(ifelse(car_test$IsBadBuy==class_test,1,0))/3019
accuracy

##this is how you train a logistic regression model

log_model <- glm(IsBadBuy~Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data=car_train,family="binomial")
summary(log_model)

##note the slight change to the predict function here
log_preds <- predict(log_model,newdata=car_test,type="response")

##do the logistic classification
log_class <- ifelse(log_preds>.5,1,0)

##make a confusion matrix
table(car_test$IsBadBuy,log_class)

accuracy1 = sum(ifelse(car_test$IsBadBuy==log_class,1,0))/3019
accuracy1




