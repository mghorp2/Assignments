
# Read Dataset
library(readr)
zip_test <- read_delim("D:/UIC/Fall 2017/Advance stats/Assignments/Assignment1/zip.test.gz", 
                       " ", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
View(zip_test)
library(readr)
zip_train <- read_delim("D:/UIC/Fall 2017/Advance stats/Assignments/Assignment1/zip.train.gz", 
                       " ", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
#View(zip_train) 

zip_train <- zip_train[,1:257]    # cleaning the data
zipTrain <- subset(zip_train,subset=X1%in%c(2,3)) #Considering 2's and 3's
#View(zipTrain)
zipTest <- subset(zip_test,subset=X1%in%c(2,3))   #Considering 2's and 3's
#view(zipTest)

# Part 1: Linear Regresion

zip_Model <- lm(X1 ~ ., zipTrain) # Linear Regression model
#print(zip_Model$coefficients)
summary(zip_Model)     
par(mfrow = c(2,2))
plot(zip_Model)

X1_pred_train<- predict(zip_Model, zipTrain)
X1_actual_train = zipTrain$X1
X1_train <- sqrt(mean((X1_actual_train - X1_pred_train)^2))          #RMSE Training 
print(X1_train)
X1pred<-sapply(X1_pred_train,function(X1_pred_train)(if (X1_pred_train >2.5 )3 else 2))
table(X1pred,zipTrain$X1)                                            # plotting confusion matrix for training

X1_predict<- predict(zip_Model, zipTest)
X1_actual = zipTest$X1
X1_test <- sqrt(mean((X1_actual - X1_predict)^2)) #RMSE Test 
print(X1_test)
X1pred_test<- sapply(X1_predict,function(X1_predict)(if (X1_predict >2.5 )3 else 2))
table(X1pred_test,zipTest$X1)                                   # plotting confusion matrix for test


# Part 2: K-nearest neighbor

library(class)

knn_1_train<-knn(train=zipTrain[,2:257],test=zipTrain[,2:257],cl = as.factor(zipTrain$X1),k=1) # Knn model for K=1 
knn_1_pred <- as.numeric(as.character(knn_1_train))
knn_act = zipTrain$X1
table(knn_1_train,zipTrain$X1) # plotting confusion matrix for Knn=1 training data
RMSE_1_train <- mean(knn_act != knn_1_pred) #Errro rate of Training for K=1 
print(RMSE_1_train)


knn_1<-knn(train=zipTrain[,2:257],test=zipTest[,2:257],cl = as.factor(zipTrain$X1),k=1) # Knn model for K=1 
knn_1_predict <- as.numeric(as.character(knn_1))
summary(knn_1)
knn_actual = zipTest$X1
table(knn_1,zipTest$X1) # plotting confusion matrix for Knn=1 test data
RMSE_1_test <- mean(knn_actual != knn_1_predict) #Errro rate of Test for K=1
print(RMSE_1_test)

knn_3_train<-knn(train=zipTrain[,2:257],test=zipTrain[,2:257],cl = as.factor(zipTrain$X1),k=3) # Knn model for K=3 
str(knn_3_train)
knn_3_pred <- as.numeric(as.character(knn_3_train))
table(knn_3_train,zipTrain$X1) # plotting confusion matrix for Knn=3 training data
RMSE_3_train <- mean(knn_act != knn_3_pred) #Errro rate of Training for K=3
print(RMSE_3_train)

knn_3<-knn(train=zipTrain[,2:257],test=zipTest[,2:257],cl = as.factor(zipTrain$X1),k=3) # Knn model for K=3 
knn_3_predict <- as.numeric(as.character(knn_3))
table(knn_3,zipTest$X1) # plotting confusion matrix for Knn=3 test data
RMSE_3_test <- mean(knn_actual != knn_3_predict) #Errro rate of Test for K=3
print(RMSE_3_test)

knn_5_train<-knn(train=zipTrain[,2:257],test=zipTrain[,2:257],cl = as.factor(zipTrain$X1),k=5) # Knn model for K=5
knn_5_pred <- as.numeric(as.character(knn_5_train))
table(knn_5_train,zipTrain$X1) # plotting confusion matrix for Knn=5 training data
RMSE_5_train <- mean(knn_act != knn_5_pred) #Errro rate of Training for K=5
print(RMSE_5_train)

knn_5<-knn(train=zipTrain[,2:257],test=zipTest[,2:257],cl = as.factor(zipTrain$X1),k=5) # Knn model for K=5 
knn_5_predict <- as.numeric(as.character(knn_5))
table(knn_5,zipTest$X1) # plotting confusion matrix for Knn=5 test data
RMSE_5_test <- mean(knn_actual != knn_5_predict) #Errro rate of Test for K=5
print(RMSE_5_test)


knn_7_train<-knn(train=zipTrain[,2:257],test=zipTrain[,2:257],cl = as.factor(zipTrain$X1),k=7) # Knn model for K=7
knn_7_pred <- as.numeric(as.character(knn_7_train))
table(knn_7_train,zipTrain$X1) # plotting confusion matrix for Knn=7 training data
RMSE_7_train <- mean(knn_act != knn_7_pred) #Errro rate of Training for K=7
print(RMSE_7_train)

knn_7<-knn(train=zipTrain[,2:257],test=zipTest[,2:257],cl = as.factor(zipTrain$X1),k=7) # Knn model for K=7 
knn_7_predict <- as.numeric(as.character(knn_7))
table(knn_7,zipTest$X1) # plotting confusion matrix for Knn=7 test data
RMSE_7_test <- mean(knn_actual != knn_7_predict) #Errro rate of Test for K=7
print(RMSE_7_test)

knn_15_train<-knn(train=zipTrain[,2:257],test=zipTrain[,2:257],cl = as.factor(zipTrain$X1),k=15) # Knn model for K=15 
knn_15_pred <- as.numeric(as.character(knn_15_train))
table(knn_15_train,zipTrain$X1) # plotting confusion matrix for Knn=15 training data
RMSE_15_train <- mean(knn_act != knn_15_pred) #Errro rate of Training for K=15
print(RMSE_15_train)


knn_15<-knn(train=zipTrain[,2:257],test=zipTest[,2:257],cl = as.factor(zipTrain$X1),k=15) # Knn model for K=15 
knn_15_predict <- as.numeric(as.character(knn_15))
table(knn_15,zipTest$X1) # plotting confusion matrix for Knn=15 test data
RMSE_15_test <- mean(knn_actual != knn_15_predict) #Errro rate of Test for K=7
print(RMSE_15_test)