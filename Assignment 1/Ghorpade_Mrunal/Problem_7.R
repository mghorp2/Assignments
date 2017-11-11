library(class)

# Create Dataset
X1 <- c(0,2,0,0,-1,1)
X2 <- c(3,0,1,1,0,1)
X3 <- c(0,0,3,2,1,1)
Y  <- c("R","R","R","G","G","R") 

training <- data.frame(X1, X2, X3, Y)
training
testset  <- data.frame(0,0,0)			# New Observation



# finding out Euclidean Distance from each observation

X1 <- c(0,0,0,0,0,0)
X2 <- c(0,0,0,0,0,0)
X3 <- c(0,0,0,0,0,0)

test<- data.frame(X1,X2,X3) # creating 6x3 Matrix for calculating Eucildean distance
print(test)
train<- training[,1:3]
print(train)
euc_dis<-as.vector(sqrt(rowSums((train-test)^2)))  # calculating Euclidean distance from training data and test point
names(euc_dis)<-c("R","R","R","G","G","R")         # Giving names of color to the point from which the distance is calculated
print(euc_dis)


# Prediction with knn=1

knn_1<- knn(train = training[,1:3],test = testset, k=1, cl=as.factor(Y)) 
print(knn_1) 

# Prediction with knn=3

knn_3<- knn(train = training[,1:3],test = testset, k=3, cl=as.factor(Y)) 
print(knn_3)
