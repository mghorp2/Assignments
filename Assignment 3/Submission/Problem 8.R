library(ISLR)
library(tree)
library (randomForest)

CS<-data.frame(Carseats)
summary(CS)
str(CS)
head(CS)

# Spliting the data into Training and Test data
set.seed(25)
train = sample (1: nrow(CS ), nrow(CS )/2)
CS_train<-CS[train,]
CS_test<-CS[-train,]

# Fitting a Regression Tree
tree.CS <- tree(Sales ~ ., data = CS_train)
summary(tree.CS)
plot(tree.CS)
text(tree.CS, pretty = 0)
yhat<-predict(tree.CS,newdata=CS_test)
MSE<-mean((yhat - CS_test$Sales)^2)
MSE

#Cross Validation
set.seed(25)
cv.CS<-cv.tree(tree.CS)
plot(cv.CS$size, cv.CS$dev, type = "b")

prune.CS <- prune.tree(tree.CS, best = 10)
plot(prune.CS)
text(prune.CS, pretty = 0)
yhat <- predict(prune.CS, newdata = CS_test)
prune_MSE<-mean((yhat - CS_test$Sales)^2)
prune_MSE

# Bagging Approch
set.seed(25)
Bag.CS <- randomForest(Sales ~ ., data = CS_train, mtry = 10, ntree = 500, importance = TRUE)
yhat.Bag <- predict(Bag.CS, newdata = CS_test)
mean((yhat.Bag - CS_test$Sales)^2)
importance(Bag.CS)

#Random Forest
set.seed(25)
RF.CS <- randomForest(Sales ~ ., data = CS_train, mtry = 3, ntree = 500, importance = TRUE)
yhat.RF <- predict(RF.CS, newdata = CS_test)
MSE<-mean((yhat.RF - CS_test$Sales)^2)
MSE
importance(RF.CS)

RF.CS <- randomForest(Sales ~ ., data = CS_train, mtry = 2, ntree = 500, importance = TRUE)
yhat.RF <- predict(RF.CS, newdata = CS_test)
mean((yhat.RF - CS_test$Sales)^2)

RF.CS <- randomForest(Sales ~ ., data = CS_train, mtry = 5, ntree = 500, importance = TRUE)
yhat.RF <- predict(RF.CS, newdata = CS_test)
mean((yhat.RF - CS_test$Sales)^2)

RF.CS <- randomForest(Sales ~ ., data = CS_train, mtry = 8, ntree = 500, importance = TRUE)
yhat.RF <- predict(RF.CS, newdata = CS_test)
mean((yhat.RF - CS_test$Sales)^2)

RF.CS <- randomForest(Sales ~ ., data = CS_train, mtry = 9, ntree = 500, importance = TRUE)
yhat.RF <- predict(RF.CS, newdata = CS_test)
mean((yhat.RF - CS_test$Sales)^2)

RF.CS <- randomForest(Sales ~ ., data = CS_train, mtry = 10, ntree = 500, importance = TRUE)
yhat.RF <- predict(RF.CS, newdata = CS_test)
mean((yhat.RF - CS_test$Sales)^2)



