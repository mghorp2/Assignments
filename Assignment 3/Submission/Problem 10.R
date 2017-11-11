library(ISLR)
library(gbm)
library(glmnet)
library(randomForest)

Hitters <- data.frame(Hitters)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)
summary(Hitters)
train <- 1:200
Hitters_train <- Hitters[train, ]
Hitters_test <- Hitters[-train, ]

set.seed(42)
pow <- seq(-10, -0.2, by = 0.1)
lambda <- as.numeric(10^pow)
Train_MSR<-vector(mode="list",length = length(lambda))
Test_MSR<-vector(mode="list",length = length(lambda))
j=1
for (j in 1:length(lambdas))
{
  boost_hit_train=gbm(Salary~.,data=Hitters_train,distribution = "gaussian",n.trees = 1000,shrinkage = lambda[j])
  yhat.boost=predict (boost_hit_train ,newdata =Hitters_train,n.trees =1000)
  Train_MSR[j]<- mean(( yhat.boost -Hitters_train$Salary)^2)
  yhat.boost_test=predict (boost_hit_train ,newdata =Hitters_test,n.trees =1000)
  Test_MSR[j]<- mean(( yhat.boost_test -Hitters_test$Salary)^2)
  j=j+1
}
plot(lambda,Train_MSR,type = "b",xlab = "Shrinkage values", ylab = "Training MSE")
plot(lambda,Test_MSR,type = "b",xlab = "Shrinkage values", ylab = "Test MSE")
Min_Train<-min(as.numeric(Train_MSR))
Min_Train
Train_lambda<-lambda[which.min(as.numeric(Train_MSR))]
Train_lambda
Min_Test<-min(as.numeric(Test_MSR))
Min_Test
Test_lambda<-lambda[which.min(as.numeric(Test_MSR))]
Test_lambda
set.seed(42)
boost.hit <- gbm(Salary ~ ., data = Hitters_train, distribution = "gaussian", n.trees = 1000, shrinkage = Test_lambda) 
summary(boost.hit)  #to see which variables are important


# Predicting salary using Ridge regression
x_train=model.matrix (Salary~.,Hitters_train)[,-1]
y_train=Hitters_train$Salary
grid =10^ seq (10,-2, length =100)
ridge.mod =glmnet (x_train,y_train,alpha =0, lambda =grid)
set.seed(42)
cv.out=cv.glmnet(x_train,y_train,alpha =0)
bestlam =cv.out$lambda.min
bestlam
x_test=model.matrix (Salary~.,Hitters_test)[,-1]
y_test=Hitters_test$Salary
ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x_test)
ridge_MSE_test<-mean(( ridge.pred -y_test)^2)
ridge_MSE_test

#Predicting Salary using linear regression
train_lm <- lm(Salary ~ ., data = Hitters_train)
pred_lm <- predict(train_lm, Hitters_test)
lm_MSE<-mean((pred_lm - Hitters_test$Salary)^2)
lm_MSE

# applying Bagging
set.seed(42)
bag_hitters<-randomForest(Salary~.,data=Hitters_train,mtry=19,ntree=1000)
yhat_bag<-predict(bag_hitters,newdata=Hitters_test)
MSE_bag<-mean((yhat_bag-Hitters_test$Salary)^2)
MSE_bag

