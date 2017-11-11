# Read data 

library(MASS)
?Boston
Boston<-data.frame(Boston)
dim(Boston)
head(Boston)
str(Boston)
summary(Boston)

# part b: pairwise Scatterplots

pairs(Boston)
pairs(~crim+age+dis+black+medv,Boston)
pairs(~zn+indus+nox+age+lstat,Boston)

# part c: predictors associated with crime rate
par(mfrow=c(2,3))
plot(Boston$age, Boston$crim, main = "Age vs Crime")
plot(Boston$dis, Boston$crim, main = "Distance vs Crime")
plot(Boston$black, Boston$crim, main = "Black Population vs Crime")
plot(Boston$medv, Boston$crim, main = "Homes occupied by owners vs Crime")
plot(Boston$lstat, Boston$crim, main = "Low status vs Crime")
plot(Boston$ptratio, Boston$crim, main = "Pupil-Teachers ration vs Crime")

Sub_Boston<-(Boston[Boston$crim>4,]) #selecting suburbs who have crime rate > 4
dim(Sub_Boston)
nrow(Sub_Boston[Sub_Boston$black>200,])/nrow(Sub_Boston) # percentage of black population where crime rate > 4
range(Boston[Boston$crim>4,]$tax)
range(Boston[Boston$crim>4,]$ptratio)
range(Boston[Boston$crim>4,]$lstat)

# part d
par(mfrow=c(2,2))
hist(Boston$crim)
boxplot(Boston$crim)
nrow(Boston[Boston$crim>20,])
nrow(Boston[Boston$crim>6,])/nrow(Boston)
range(Boston$tax)
hist(Boston$tax)
range(Boston[Boston$crim>20,]$tax)
range(Boston$ptratio)
hist(Boston$ptratio)
range(Boston[Boston$crim>20,]$ptratio)

# Part e: find # of suburbs in this data set bound to the Charles river

nrow(Boston[Boston$chas==1,]) # number of suburbs near charles river

# Part f: median pupil-teacher ratio among the towns

median(Boston$ptratio)

# Part g 

summary(Boston[Boston$medv==min(Boston$medv),])

# part h

nrow(Boston[Boston$rm>7,])
nrow(Boston[Boston$rm>8,])
summary(Boston[Boston$rm>8,])


