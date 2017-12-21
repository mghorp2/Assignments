library(e1071)

x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
Y = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(x1, x2, col = Y, xlim = c(0, 5), ylim = c(0, 5))
df<-cbind.data.frame(x1,x2,Y)

set.seed(1)
svmfit =svm(Y~., data=df, kernel ="linear", cost =5,scale = FALSE,fitted=TRUE)
summary(svmfit)
table(svmfit$fitted, df$Y)
# Extract beta_0 and beta_1
beta0 = svmfit$rho
beta = drop(t(svmfit$coefs) %*% as.matrix(df[svmfit$index,1:2]))
# Replot, this time with the solid line representing the maximal margin plane.
plot(x1, x2, col=Y, pch=19, data=df)
abline(beta0/beta[2], -beta[1]/beta[2])
paste("Intercept: ", round(beta0/beta[2],1), ", Slope: ", round(-beta[1]/beta[2],1), sep="")

plot(x1, x2, col=Y, pch=19, data=df)
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 2)
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 2)

d = abs(beta0/beta[2] - (beta0 + 1)/beta[2]) / sqrt((-beta[1]/beta[2])^2+1)
names(d) <- "Margin Width"
d


set.seed(1)
svmfit =svm(Y~., data=df, kernel ="linear", cost =1,scale = FALSE,fitted=TRUE)
summary(svmfit)
table(svmfit$fitted, df$Y)
# Extract beta_0 and beta_1
beta0 = svmfit$rho
beta = drop(t(svmfit$coefs) %*% as.matrix(df[svmfit$index,1:2]))
# Replot, this time with the solid line representing the maximal margin plane.
plot(x1, x2, col=Y, pch=19, data=df)
abline(beta0/beta[2], -beta[1]/beta[2])
paste("Intercept: ", round(beta0/beta[2],1), ", Slope: ", round(-beta[1]/beta[2],1), sep="")


plot(x1, x2, col=Y, pch=19, data=df)
points(2, 3, col="blue", pch=19)
points(2, 3, col="blue", pch=5, cex=2)


