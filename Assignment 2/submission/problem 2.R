library(MASS)
Boston<- data.frame(Boston)
summary(Boston)
str(Boston)
dim(Boston)

# simple linear regression

predictor<-c("zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")

summ <- data.frame("Predictor"=character(),"Estimate"=numeric(),"Std_error"=numeric(),"t_value"=numeric(),"p_value"=numeric(),stringsAsFactors = FALSE) # Create vector
 
j=1

for (i in 1:13)
{
model <- lm(crim ~ eval(parse(text=predictor[i])),data=Boston)
model1<-summary(model)
summ[j,]=c(predictor[i],model1$coefficients[2,])
j=j+1
par(mfrow=c(2,2))
plot(model)
}
print(summ)
summ[,2]

# multiple linear regression

lm_multi<-lm(crim ~ .,data = Boston)
lm_m<-summary(lm_multi)
summ_multi<-lm_m$coefficients[2:14,1]
str(summ_multi)

#plot simple vs multiple
sm<-data.frame(summ[,1:2],summ_multi)
#sm$Estimate<-as.numeric(sm$Estimate)
plot(x=sm$Estimate,y=sm$summ_multi,xlab="Coef for Simple Linear Regression", ylab="Coef for Multiple Linear Regression")
text(x=sm$Estimate, y=sm$summ_multi, labels=sm$Predictor, cex=.7, col="blue", pos=4)

# find out nonlinear association 

sum_nl <- data.frame("Predictor"=character(),"Estimate"=numeric(),"Std_error"=numeric(),"t_value"=numeric(),"p_value"=numeric(),stringsAsFactors = FALSE) # Create vector
j=1

col_n <-c("zn","indus","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")

for (i in 1:12){
 model_nl <- lm(crim ~ poly(eval(parse(text=col_n[i])),3),data=Boston)
  model_sum_n1<-summary(model_nl)
  #print(model_sum_n1)
  for (k in 2:4){
    sum_nl[j,]=c(col_n[i],summary(model_nl)$coefficients[k,])
    j = j+1
  }
}
print(sum_nl)


