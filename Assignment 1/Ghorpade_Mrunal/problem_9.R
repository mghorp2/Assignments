library(corrplot)

# Read Dataset
Auto <- read.csv("D:/UIC/Fall 2017/Advance stats/Assignments/Assignment1/Auto.csv")
Auto <- read.csv(file.choose())
view(Auto)

# Describe Dataset
str(Auto)			# Check the type of each variable!
summary(Auto)		# Numerical Summaries
dim(Auto)			# How many observations are there?
head(Auto)

plot(Auto) # Scatterplot

Auto$horsepower <- as.numeric(Auto$horsepower) # Change the type of variable
cor(Auto[,-9])

corrplot(cor(Auto[,-9],use="complete.obs"),type="lower") # plotting correlation
			
# Linear Regression
     Auto_model <- lm(mpg ~ ., Auto[,-9])
     view(Auto_model)
     summary(Auto_model)		# Summary statistics for the linear reg
     par(mfrow = c(2,2))	# Put Multiple Plots on a Single Page
     plot(Auto_model)				# Diagnostic plot of the linear regression
     
 
# part 2 studing the interaction effect    
     Auto_inter <- lm(mpg ~ .+weight*year+horsepower:acceleration+cylinders:displacement, Auto[,-9])
     summary(Auto_inter)

# part 3    
     Auto_trans <- lm(mpg ~ .+log(weight)+I(acceleration^2), Auto[,-9])
     summary(Auto_trans)
     plot(Auto_trans)
     