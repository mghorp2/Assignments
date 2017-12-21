library(ISLR)
library(caret)
library(e1071)

auto<-data.frame(Auto)  
str(auto)
summary(auto)
median_mpg<-median(auto$mpg)
auto$mpg_l<-NA
for (i in 1:nrow(auto)){
  if(auto$mpg[i]>=median_mpg){
    auto$mpg_l[i]<-1}
  else
  {auto$mpg_l[i]<-0}
}
auto$mpg_l<-as.factor(auto$mpg_l)

head(auto,2)

set.seed(1)
tune.out <- tune(svm, mpg_l ~ ., data = auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

pred=predict(tune.out$best.model,auto)
conf_matrix_linear<-confusionMatrix(pred,auto$mpg_l)
conf_matrix_linear

set.seed(1)
tune.out_rad <- tune(svm, mpg_l ~ ., data = auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100),gamma=c(0.5,1,2,3,4)))
summary(tune.out_rad)
pred_rad=predict(tune.out_rad$best.model,auto)
conf_matrix_rad<-confusionMatrix(pred_rad,auto$mpg_l)
conf_matrix_rad

set.seed(1)
tune.out_poly <- tune(svm, mpg_l ~ ., data = auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(tune.out_poly)
pred_poly=predict(tune.out_poly$best.model,auto)
conf_matrix_poly<-confusionMatrix(pred_poly,auto$mpg_l)
conf_matrix_poly

# plots
svm.linear <- svm(mpg_l ~ ., data = auto, kernel = "linear", cost = 1)
svm.poly <- svm(mpg_l ~ ., data = auto, kernel = "polynomial", cost = 100, degree = 2)
svm.radial <- svm(mpg_l ~ ., data = auto, kernel = "radial", cost = 100, gamma = 0.01)


# linear Plots
plot(svm.linear, auto, mpg ~ acceleration)
plot(svm.linear, auto, mpg ~ displacement)
plot(svm.linear, auto, mpg ~ horsepower)
plot(svm.linear, auto, mpg ~ cylinders)
plot(svm.linear, auto, mpg ~ weight)
plot(svm.linear, auto, mpg ~ year)


# Radial Plots
plot(svm.radial, auto, mpg ~ acceleration)
plot(svm.radial, auto, mpg ~ displacement)
plot(svm.radial, auto, mpg ~ horsepower)
plot(svm.radial, auto, mpg ~ cylinders)
plot(svm.radial, auto, mpg ~ weight)
plot(svm.radial, auto, mpg ~ year)

# Polynomial Plots
plot(svm.poly, auto, mpg_l ~ acceleration)
plot(svm.poly, auto, mpg_l ~ displacement)
plot(svm.poly, auto, mpg_l ~ horsepower)
plot(svm.poly, auto, mpg_l ~ cylinders)
plot(svm.poly, auto, mpg_l ~ weight)
plot(svm.poly, auto, mpg_l ~ year)

